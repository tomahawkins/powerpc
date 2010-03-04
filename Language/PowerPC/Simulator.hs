-- | PowerPC instruction set simulation.
module Language.PowerPC.Simulator
  ( Memory (..)
  , simulate
  ) where

import Control.Monad
import Data.Bits
import Data.List
import Data.Word
import System.IO
import Text.Printf

import Language.PowerPC.Instructions
import Language.PowerPC.RTL

-- | Memory interface.
class Memory a where
  load  :: a -> Word64 -> Int -> IO [Word8]
  store :: a -> Word64 -> [Word8] -> IO ()
  fetch :: a -> Word64 -> IO Word32
  fetch mem addr = load mem addr 4 >>= return . fromBytes

-- | State of processor.
data Machine = Machine
  { pc      :: Word64
  , lr      :: Word64
  , ctr     :: Word64
  , gprs    :: [Word64]
  , xer     :: Word64
  , cr      :: Word32
  }

instance Show Machine where
  show m = unlines
    [ printf "pc = %08X  lr = %08X  cr = %08X  xer = %08X" (pc m) (lr m) (cr m) (xer m)
    , r [0..3]
    , r [4..7]
    , r [8..11]
    , r [12..15]
    , r [16..19]
    , r [20..23]
    , r [24..27]
    , r [28..31]
    ]
    where
    r ns = intercalate "  " [ printf "r%-2d = %016X" n (gprs m !! n) | n <- ns ]

-- | Run simulation given memory and the starting address of program.
simulate :: Memory a => a -> Word64 -> IO ()
simulate memory start = loop Machine
  { pc      = start
  , lr      = 0
  , ctr     = 0
  , gprs    = replicate 32 0
  , xer     = 0
  , cr      = 0
  }
  where

  loop :: Machine -> IO ()
  loop m = do
    --print m >> hFlush stdout
    next <- fetch memory $ pc m
    --let (n, s) = rtl (pc m) next
    --printf "%08X  %-6s  %s\n" (pc m) n (show s) >> hFlush stdout
    n <- step memory next m
    when (pc m + 4 /= pc n) $ printf "branched from 0x%08X to 0x%08X\n" (pc m) (pc n) >> hFlush stdout
    loop n

  --done :: Machine -> Bool
  --done m | pc m < base || pc m > base + (fromIntegral (length p) + 4) = error $ printf "program counter out of range: 0x%08X" $ pc m
         -- | otherwise                                                  = pc m == base + (fromIntegral $ length p)

step :: Memory a => a -> Word32 -> Machine -> IO Machine
step memory instr m = do
  (m, env) <- evalStmt (m, []) $ snd $ rtl (pc m) instr
  return m { pc = fromInteger $ lookup' "NIA" env }

  where

  evalStmt :: (Machine, [(String, Integer)]) -> Stmt -> IO (Machine, [(String, Integer)])
  evalStmt (m, env) a = case a of
    Seq a -> foldM evalStmt (m, env) a
    If a b c -> do
      a <- evalExpr (m, env) a
      if a /= 0 then evalStmt (m, env) b else evalStmt (m, env) c
    While cond stmt -> do
      cond' <- evalExpr (m, env) cond
      if cond' == 0
        then return (m, env)
        else do
          (m, env) <- evalStmt (m, env) stmt
          evalStmt (m, env) $ While cond stmt
    Warning msg fields -> do
      putStr ("Warning: " ++ msg)
      mapM_ (\ (format, e) -> do
        e <- evalExpr (m, env) e
        putStr "  "
        printf format (fromIntegral e :: Word64)) fields
      putStrLn ""
      return (m, env)
    Assign cond a b -> do
      b <- evalExpr (m, env) b
      m <- conditions (m, env) b cond
      let b' = fromIntegral b
      case a of
        V n -> return (m, (n, b) : env)
        --CR
        CTR -> return (m { ctr = b' }, env)
        EA  -> return (m, ("EA", b) : env)
        LR  -> return (m { lr = b' }, env)
        GPR n -> do
          n <- evalExpr (m, env) n
          return (m { gprs = replace (fromIntegral n) b' (gprs m) }, env)
        MEM addr bytes -> do
          addr <- evalExpr (m, env) addr
          store memory (fromIntegral addr) $ toBytes bytes b
          return (m, env)
        NIA -> return (m, ("NIA", b) : env)
        RA  -> return (m { gprs = replace (ufield 11 15) b' (gprs m) }, env)
        RT  -> return (m { gprs = replace (ufield  6 10) b' (gprs m) }, env)
        XER -> return (m { xer = b' }, env)
        Null -> return (m, env)
        _ -> error $ "Invalid assignment target: " ++ show a

  evalExpr :: (Machine, [(String, Integer)]) -> E -> IO Integer
  evalExpr (m, env) a = case a of
    V n       -> return $ lookup' n env
    C a       -> return $ a
    Add a b   -> binop (+) a b
    Sub a b   -> binop (-) a b
    Mul a b   -> binop (*) a b
    Not a     -> uniop (toBool . (== 0)) a
    And a b   -> binop (\ a b -> toBool $ a /= 0 && b /= 0) a b
    Or  a b   -> binop (\ a b -> toBool $ a /= 0 || b /= 0) a b
    BWNot a   -> uniop complement a
    BWAnd a b -> binop (.&.) a b
    BWOr  a b -> binop (.|.) a b
    Shift a b -> binop (\ a b -> shift a $ fromIntegral b) a b
    Eq a b    -> binop (\ a b -> toBool $ a == b) a b
    Lt a b    -> binop (\ a b -> toBool $ (fromIntegral a :: Word64) < (fromIntegral b :: Word64)) a b
    Null      -> error "Invalid expressions: Null"
    Bit a w i -> binop (\ a i -> if i >= fromIntegral w then error "Bit: bit index greater than width" else toBool $ testBit a $ w - 1 - fromIntegral i) a i
    AA        -> return $ bool 30
    BD        -> return $ clearBits (sfield 16 31) [1, 0]
    BF        -> return $ ufield  6  8
    BI        -> return $ ufield 11 15
    BO        -> return $ ufield  6 10
    CIA       -> return $ fromIntegral $ pc m
    CReg      -> return $ fromIntegral $ cr m
    CTR       -> return $ fromIntegral $ ctr m
    D         -> return $ sfield 16 31
    EA        -> return $ lookup' "EA" env
    EXTS n a  -> uniop (\ a -> if testBit a (63 - n) then a .|. complement (setBits 0 [0 .. 63 - n]) else a .&. setBits 0 [0 .. 63 - n]) a
    GPR a     -> uniop (\ a -> fromIntegral $ gprs m !! fromIntegral a) a
    L10       -> return $ bool 10
    L15       -> return $ bool 15
    LI        -> return $ clearBits (sfield 6 31) [1, 0]
    LK        -> return $ bool 31
    LR        -> return $ fromIntegral $ lr m
    MASK start stop -> binop (\ start stop -> setBits 0 [63 - fromIntegral stop .. 63 - fromIntegral start]) start stop
    MB5       -> return $ ufield 21 25
    MB6       -> return $ ufield 21 26
    ME5       -> return $ ufield 26 30
    ME6       -> return $ ufield 21 26
    MEM addr bytes -> do
      addr <- evalExpr (m, env) addr
      value <- load memory (fromIntegral addr) bytes
      return $ fromBytes value
    NIA        -> return $ lookup' "NIA" env
    OE         -> return $ bool 21
    RAI        -> return $ ufield 11 15
    RA         -> return $ fromIntegral $ gprs m !! fromIntegral (ufield 11 15)
    RB         -> return $ fromIntegral $ gprs m !! fromIntegral (ufield 16 20)
    Rc         -> return $ bool 31
    ROTL32 a n -> binop (rotateLN 32) a n
    ROTL64 a n -> binop (rotateLN 64) a n
    RS         -> return $ fromIntegral $ gprs m !! fromIntegral (ufield 6 10)
    RSI        -> return $ ufield 6 10
    RT         -> return $ fromIntegral $ gprs m !! fromIntegral (ufield 6 10)
    RTI        -> return $ ufield 6 10
    SH5        -> return $ ufield 16 20
    SH6        -> return $ shiftL (ufield 30 30) 5 .|. ufield 16 20
    SI         -> return $ sfield 16 31
    SPR        -> return $ shiftL (ufield 16 20) 5 .|. ufield 11 15
    UI         -> return $ ufield 16 31
    XER        -> return $ fromIntegral $ xer m
    where

    binop :: (Integer -> Integer -> Integer) -> E -> E -> IO Integer
    binop f a b = do
      a <- evalExpr (m, env) a
      b <- evalExpr (m, env) b
      return $ f a b

    uniop :: (Integer -> Integer) -> E -> IO Integer
    uniop f a = do
      a <- evalExpr (m, env) a
      return $ f a

  toBool :: Bool -> Integer
  toBool True  = 1
  toBool False = 0
  
  ufield :: Int -> Int -> Integer
  ufield h l = shiftR (fromIntegral instr) (31 - l) .&. setBits 0 [0 .. l - h]

  sfield :: Int -> Int -> Integer
  sfield h l = ufield h l .|. (if testBit instr (31 - h) then complement $ setBits 0 [0 .. l - h] else 0)

  bool :: Int -> Integer
  bool n = toBool $ testBit instr (31 - n)
  

  conditions :: (Machine, [(String, Integer)]) -> Integer -> [Cond] -> IO Machine
  conditions (m, env) value conds = do
    m <- foldM condition m $ sort conds
    return m
    where
    condition :: Machine -> Cond -> IO Machine
    condition n cond = case cond of
      CA c -> on c $ return n { xer = if isCA value then setBit (xer n) 29 else clearBit (xer n) 29 }
      OV c -> on c $ return n { xer = if isOV value then setBits (xer n) [31, 30] else clearBit (xer n) 30 }
      CR c field -> on c $ do
        field <- evalExpr (m, env) field
        let bits = [(0, isLT value), (1, isGT value), (2, isEQ value), (3, testBit (xer n) 31)]
            bitsToSet   = map f [ n | (n, a) <- bits, a ]
            bitsToClear = map f [ n | (n, a) <- bits, not a ]
            f i = 31 - (i + 4 * fromIntegral field)
        return n { cr = clearBits (setBits (cr n) bitsToSet) bitsToClear }
      where
      on :: E -> IO Machine -> IO Machine
      on c action = do
        c <- evalExpr (m, env) c
        if c /= 0 then action else return n

isLT :: Integer -> Bool
isLT a = testBit a 63

isEQ :: Integer -> Bool
isEQ a = (fromIntegral a :: Word64) == 0

isGT :: Integer -> Bool
isGT a = not (isLT a) && not (isEQ a)

isCA :: Integer -> Bool
isCA a = testBit a 64

isOV :: Integer -> Bool
isOV a = testBit a 64 /= testBit a 63



replace :: Integral b => b -> a -> [a] -> [a]
replace i _ _ | i >= 32 = error "replace: index out of range"
replace _ a [] = [a]
replace i a (b:c) | i <= 0    = a : c
                  | otherwise = b : replace (i - 1) a c

toBytes :: (Integral a, Bits a) => Int -> a -> [Word8]
toBytes n _ | n <= 0 = []
toBytes n d = fromIntegral (shiftR d ((n - 1) * 8)) : toBytes (n - 1) d

fromBytes :: (Integral a, Bits a) => [Word8] -> a
fromBytes = f . reverse
  where
  f [] = 0
  f (a:b) = fromIntegral a .|. shiftL (f b) 8

lookup' :: String -> [(String, b)] -> b
lookup' a table = case lookup a table of
  Just b -> b
  Nothing -> error $ "Variable name not found: " ++ a

clearBits :: Bits a => a -> [Int] -> a
clearBits = foldl clearBit

setBits :: Bits a => a -> [Int] -> a
setBits = foldl setBit

rotateLN :: Int -> Integer -> Integer -> Integer
rotateLN carryBit a n | n <= 0 = a
                      | otherwise = rotateLN carryBit (if testBit a' carryBit then setBit a' 0 else a') (n - 1)
  where
  a' = shiftL a 1

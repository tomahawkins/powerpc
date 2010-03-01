-- | PowerPC instruction set simulation.
module Language.PowerPC.Simulator
  ( Memory (..)
  , simulate
  ) where

import Control.Monad
import Data.Bits
import Data.Word
import Text.Printf

import Language.PowerPC.Instructions
import Language.PowerPC.RTL

-- | Memory interface.
class Memory a where
  load  :: a -> Word64 -> Int -> IO [Word8]
  store :: a -> Word64 -> [Word8] -> IO ()
  fetch :: a -> Word64 -> IO Word32
  fetch mem addr = load mem addr 4 >>= return . fromBytes

data Machine = Machine
  { pc      :: Word64
  , lr      :: Word64
  , ctr     :: Word64
  , gprs    :: [Word64]
  , xer     :: Word64
  , cr      :: Word32
  }

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
    next <- fetch memory $ pc m
    n <- step memory next m
    when (pc m + 4 /= pc n) $ printf "branched to: 0x%08X\n" $ pc n
    loop n

  --done :: Machine -> Bool
  --done m | pc m < base || pc m > base + (fromIntegral (length p) + 4) = error $ printf "program counter out of range: 0x%08X" $ pc m
         -- | otherwise                                                  = pc m == base + (fromIntegral $ length p)

step :: Memory a => a -> Word32 -> Machine -> IO Machine
step memory instr m = do
  (m, env) <- evalStmt (m, []) $ rtl (pc m) instr
  return m { pc = lookup' "NIA" env }

  where

  evalStmt :: (Machine, [(String, Word64)]) -> Stmt -> IO (Machine, [(String, Word64)])
  evalStmt (m, env) a = case a of
    Seq a b -> do
      a <- evalStmt (m, env) a
      evalStmt a b
    Null -> return (m, env)
    If a b c -> do
      (a, m) <- evalExpr (m, env) a
      if a /= 0 then evalStmt (m, env) b else evalStmt (m, env) c
    Warning msg -> putStrLn ("Warning: " ++ msg) >> return (m, env)
    Assign a b -> do
      (b, m) <- evalExpr (m, env) b
      case a of
        V n -> return (m, (n, b) : env)
        --CR
        --CTR
        --EA
        EA  -> return (m, ("EA", b) : env)
        --LR
        --MEM E Int
        NIA -> return (m, ("NIA", b) : env)
        --RA
        --RT
        --XER
        _ -> error $ "Invalid assignment target: " ++ show a

  evalExpr :: (Machine, [(String, Word64)]) -> E -> IO (Word64, Machine)
  evalExpr (m, env) a = case a of
    V n       -> return (lookup' n env, m)
    C a       -> return (a, m)
    Add a b   -> binop (+) a b
    Sub a b   -> binop (-) a b
    Not a     -> uniop (toBool . (== 0)) a
    And a b   -> binop (\ a b -> toBool $ a /= 0 && b /= 0) a b
    Or  a b   -> binop (\ a b -> toBool $ a /= 0 || b /= 0) a b
    BWNot a   -> uniop complement a
    BWAnd a b -> binop (.&.) a b
    BWOr  a b -> binop (.|.) a b
    Shift a b -> binop (\ a b -> shift a $ fromIntegral b) a b
    Eq a b    -> binop (\ a b -> toBool $ a == b) a b
    Bit a w i -> binop (\ a i -> toBool $ testBit a $ w - 1 - fromIntegral i) a i
    Cond _ a  -> uniop id a --XXX
    AA        -> return (bool 30, m)
    --BD
    --BI
    --BO
    --CIA
    --CR
    --CTR
    --D
    --EA
    --LI
    --LK
    --LR
    --MEM E Int
    --NIA
    --OE
    --RAI
    --RA
    --RB
    --Rc
    --RS
    --RT
    --SI
    --SPR
    --UI
    --XER
    where
    binop :: (Word64 -> Word64 -> Word64) -> E -> E -> IO (Word64, Machine)
    binop f a b = do
      (a, m) <- evalExpr (m, env) a
      (b, m) <- evalExpr (m, env) b
      return (f a b, m)

    uniop :: (Word64 -> Word64) -> E -> IO (Word64, Machine)
    uniop f a = do
      (a, m) <- evalExpr (m, env) a
      return (f a, m)

    toBool :: Bool -> Word64
    toBool True  = 1
    toBool False = 1
    
    field :: Int -> Int -> Word64
    field h l = shiftR (fromIntegral instr) (31 - l) .&. foldl setBit 0 [0 .. l - h] 

    bool :: Int -> Word64
    bool n = toBool $ testBit instr (31 - n)
  
{-
  where

  field :: Int -> Int -> Word64
  field h l = shiftR (fromIntegral instr) (31 - l) .&. foldl setBit 0 [0 .. l - h] 

  bool :: Int -> Bool
  bool n = testBit instr (31 - n)

  exts :: Int -> Word64 -> Word64
  exts n w = if testBit w (63 - n) then foldl setBit 0 [63 - n + 1 .. 63]  .|. w else w

  exts32 :: Int -> Word64 -> Word64
  exts32 n =  exts (32 + n)

  n = m { pc = pc m + 4 }

  set :: Machine -> Word64 -> Word64 -> Machine
  set m i v = m { gprs = replace i v $ gprs m }

  setOV :: Machine -> Machine
  setOV m = m { xer = setBit (setBit (xer m) 31) 30 }

  clearOV :: Machine -> Machine
  clearOV m = m { xer = clearBit (xer m) 30 }

  setCR :: Machine -> Machine
  setCR m = m { xer = setBit (xer m) 29 }

  clearCR :: Machine -> Machine
  clearCR m = m { xer = clearBit (xer m) 29 }

  -- Assumes SO has already been set in XER.
  cr0 :: Machine -> Word64 -> Machine
  cr0 m a = m { cr = cr m .&. 0x0FFFFFFF .|. (if lt then bit 31 else 0) .|. (if gt then bit 30 else 0) .|. (if eq then bit 29 else 0) .|. (if so then bit 28 else 0) }
    where
    lt :: Bool
    lt = testBit a 63
    gt :: Bool
    gt = not eq && not lt
    eq :: Bool
    eq = a == 0
    so :: Bool
    so = testBit (xer m) 31


  aa   = bool 30
  bi   = field 11 15
  bo :: Int -> Bool
  bo i = bool $ 6 + i
  crbi :: Machine -> Bool
  crbi m = testBit (cr m) $ 31 - fromIntegral bi
  d    = exts32 16 $ field 16 31
  li   = exts32  6 $ field 6 31 .&. complement 0x3
  lk   = bool 31
  oe   = bool 21
  ra   = gprs m !! fromIntegral rai
  rai  = field 11 15
  rb   = gprs m !! fromIntegral rbi
  rbi  = field 16 20
  rc   = bool 31
  rs   = gprs m !! fromIntegral rsi
  rsi  = field 6 10
  rti  = field 6 10
  si   = exts32 16 $ field 16 31
  spr  = shiftL (field 16 20) 5 .|. field 11 15
  ui   = field 16 31
-}






replace :: Integral b => b -> a -> [a] -> [a]
replace _ a [] = [a]
replace i a (b:c) | i <= 0    = a : c
                  | otherwise = b : replace (i - 1) a c

toBytes :: Int -> Word64 -> [Word8]
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

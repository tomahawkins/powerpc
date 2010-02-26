-- | General tools for PowerPC programs.
module Language.PowerPC.Simulator
  ( Memory (..)
  , simulate
  ) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Word
import Text.Printf

import Language.PowerPC.Instruction
import Language.PowerPC.Disassembler

data Machine = Machine
  { program :: M.Map Word64 (Word32, Instruction)
  , pc      :: Word64
  , lr      :: Word64
  , ctr     :: Word64
  , gprs    :: [Word64]
  , xer     :: Word64
  , cr      :: Word32
  , ea      :: Word64
  , memory  :: Memory
  }

data Memory = Memory
  { load  :: Word64 -> Int -> IO [Word8]
  , store :: Word64 -> [Word8] -> IO ()
  }

simulate :: Word64 -> Word64 -> BS.ByteString -> Memory -> IO ()
simulate base start p m = loop Machine
  { program = M.fromList $ zip [fromIntegral base, fromIntegral base + 4 ..] $ disassemble p
  , memory = m
  , pc      = start
  , lr      = 0
  , ctr     = 0
  , gprs    = replicate 32 0
  , xer     = 0
  , cr      = 0
  , ea      = 0
  }
  where

  loop :: Machine -> IO ()
  loop m = when (not $ done m) $ do
    printf "program counter = 0x%08X  r3 = 0x%08X  r12 = 0x%08X\n" (pc m) (gprs m !! 3) (gprs m !! 12)
    step m >>= loop

  done :: Machine -> Bool
  done m | pc m < base || pc m > base + (fromIntegral (BS.length p) + 4) = error $ printf "program counter out of range: 0x%08X" $ pc m
         | otherwise                                                     = pc m == base + (fromIntegral $ BS.length p)

step :: Machine -> IO Machine
step m = case program m M.! pc m of
  (_, IUnknown opcd xo) -> error $ printf "unknown instruction:  address: 0x%08X  opcode: %d  extended opcode: %d" (pc m) opcd xo
  (i, I _ _ _ a) -> foldM (act i) m (if null [ () | PC := _ <- a ] then a ++ [PC := Reg PC + 4] else a)

act :: Word32 -> Machine -> Action -> IO Machine
act i m a = case a of
  r := e -> case r of
    PC  -> return m { pc  = eval e }
    CR  -> return m { cr  = fromIntegral $ eval e }
    LR  -> return m { lr  = eval e }
    CTR -> return m { ctr = eval e }
    XER -> return m { xer = eval e }
    RA  -> return m { gprs = replace (fromIntegral $ field 11 15) (eval e) (gprs m) }
    RT  -> return m { gprs = replace (fromIntegral $ field  6 10) (eval e) (gprs m) }
    EA  -> return m { ea = eval e }
  Load1 -> do d <- load (memory m) (ea m) 1; return m { gprs = replace (fromIntegral $ field  6 10) (fromBytes d) (gprs m) }
  Load2 -> do d <- load (memory m) (ea m) 2; return m { gprs = replace (fromIntegral $ field  6 10) (fromBytes d) (gprs m) }
  Load4 -> do d <- load (memory m) (ea m) 4; return m { gprs = replace (fromIntegral $ field  6 10) (fromBytes d) (gprs m) }
  Load8 -> do d <- load (memory m) (ea m) 8; return m { gprs = replace (fromIntegral $ field  6 10) (fromBytes d) (gprs m) }
  Store1 d -> store (memory m) (ea m) (toBytes 1 $ eval d) >> return m
  Store2 d -> store (memory m) (ea m) (toBytes 2 $ eval d) >> return m
  Store4 d -> store (memory m) (ea m) (toBytes 4 $ eval d) >> return m
  Store8 d -> store (memory m) (ea m) (toBytes 8 $ eval d) >> return m
  where

  toBytes :: Int -> Word64 -> [Word8]
  toBytes n _ | n <= 0 = []
  toBytes n d = fromIntegral (shiftR d ((n - 1) * 8)) : toBytes (n - 1) d

  fromBytes :: [Word8] -> Word64
  fromBytes = f . reverse
    where
    f [] = 0
    f (a:b) = fromIntegral a .|. shiftL (f b) 8

  field :: Int -> Int -> Word64
  field h l = shiftR (fromIntegral i) (31 - l) .&. foldl setBit 0 [0 .. l - h] 

  exts :: Int -> Word64 -> Word64
  exts n w = if testBit w (63 - n) then foldl setBit 0 [63 - n + 1 .. 63]  .|. w else w

  eval :: E -> Word64
  eval a = case a of
    Const a -> a
    Reg a -> case a of
      PC  -> pc  m
      CR  -> fromIntegral $ cr  m
      LR  -> lr  m
      CTR -> ctr m
      XER -> xer m
      RA  -> gprs m !! fromIntegral (field 11 15)
      RT  -> gprs m !! fromIntegral (field  6 10)
      EA  -> ea m
    Add a b -> eval a + eval b
    Sub a b -> eval a + eval b
    Mul a b -> eval a * eval b  --XXX Signedness?
    Not a   -> complement $ eval a
    And a b -> eval a .&. eval b
    Or  a b -> eval a .|. eval b
    Eq  a b -> if eval a == eval b then 1 else 0
    Lt  a b -> if eval a <  eval b then 1 else 0  --XXX Signedness?
    Shift a n -> shift (eval a) n
    If a b c -> if eval a /= 0 then eval b else eval c
    Exts n a -> exts n $ eval a
    AA   -> field 30 30
    D    -> exts 16 $ field 16 31
    LI   -> exts  6 $ field 6 31 .&. complement 0x3
    LK   -> field 31 31
    RAI  -> field 11 15
    RB   -> gprs m !! fromIntegral (field 16 20)
    RS   -> gprs m !! fromIntegral (field  6 10)
    SI   -> exts 16 $ field 16 31
    SPR  -> shiftL (field 16 20) 5 .|. field 11 15
    UI   -> field 16 31
  
replace :: Int -> a -> [a] -> [a]
replace _ a [] = [a]
replace i a (b:c) | i <= 0    = a : c
                  | otherwise = b : replace (i - 1) a c

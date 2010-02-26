-- | General tools for PowerPC programs.
module Language.PowerPC.Simulator
  ( Memory (..)
  , zeroMemory
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
  , memory  :: Memory
  }

data Memory = Memory
  { load1  :: Word64 -> IO Word8
  , load2  :: Word64 -> IO Word16
  , load4  :: Word64 -> IO Word32
  , load8  :: Word64 -> IO Word64
  , store1 :: Word64 -> Word8  -> IO ()
  , store2 :: Word64 -> Word16 -> IO ()
  , store4 :: Word64 -> Word32 -> IO ()
  , store8 :: Word64 -> Word64 -> IO ()
  }

zeroMemory :: Memory
zeroMemory = Memory
  { load1  = \ a   -> printf "load  0x%08X\n" a >> return 0
  , load2  = \ a   -> printf "load  0x%08X\n" a >> return 0
  , load4  = \ a   -> printf "load  0x%08X\n" a >> return 0
  , load8  = \ a   -> printf "load  0x%08X\n" a >> return 0
  , store1 = \ a d -> printf "store 0x%08X 0x%02X\n"  a d
  , store2 = \ a d -> printf "store 0x%08X 0x%04X\n"  a d
  , store4 = \ a d -> printf "store 0x%08X 0x%08X\n"  a d
  , store8 = \ a d -> printf "store 0x%08X 0x%016X\n" a d
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
  r := e' -> case r of
    PC  -> return m { pc  = e }
    CR  -> return m { cr  = fromIntegral e }
    LR  -> return m { lr  = e }
    CTR -> return m { ctr = e }
    XER -> return m { xer = e }
    RA  -> return m { gprs = replace (fromIntegral $ field 11 15) e (gprs m) }
    RT  -> return m { gprs = replace (fromIntegral $ field  6 10) e (gprs m) }
    where
    e = eval e'
  Store1 a d -> store1 (memory m) (eval a) (fromIntegral $ eval d) >> return m
  Store2 a d -> store2 (memory m) (eval a) (fromIntegral $ eval d) >> return m
  Store4 a d -> store4 (memory m) (eval a) (fromIntegral $ eval d) >> return m
  Store8 a d -> store8 (memory m) (eval a) (               eval d) >> return m
  where

  field :: Int -> Int -> Word64
  field h l = shiftR (fromIntegral i) (31 - l) .&. foldl setBit 0 [0 .. fromIntegral l - fromIntegral h] 

  bit :: Int -> Bool
  bit n = testBit i (31 - n)

  eval :: E -> Word64
  eval a = case a of
    Const a -> a
    Reg a -> case a of
      PC  -> pc  m
      CR  -> fromIntegral $ cr  m
      LR  -> lr  m
      CTR -> ctr m
      XER -> xer m
      --MEM2 a -> 
      RA  -> gprs m !! fromIntegral (field 11 15)
      RT  -> gprs m !! fromIntegral (field  6 10)
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
    AA   -> field 30 30
    D    -> (if bit 16 then 0xFFFFFFFF00000000 else 0) .|. field 16 31
    LI   -> (if bit  6 then 0xFFFFFFFFFC000000 else 0) .|. field 6 31 .&. complement 0x3
    LK   -> field 31 31
    RAI  -> field 11 15
    RB   -> gprs m !! fromIntegral (field 16 20)
    RS   -> gprs m !! fromIntegral (field  6 10)
    SI   -> (if bit 16 then 0xFFFFFFFF00000000 else 0) .|. field 16 31
    SPR  -> shiftL (field 16 20) 5 .|. field 11 15
    UI   -> field 16 31
  
replace :: Int -> a -> [a] -> [a]
replace _ a [] = [a]
replace i a (b:c) | i <= 0    = a : c
                  | otherwise = b : replace (i - 1) a c

-- | General tools for PowerPC programs.
module Language.PowerPC.Simulator
  ( simulate
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
  }

simulate :: Word64 -> Word64 -> BS.ByteString -> IO ()
simulate base start p = loop Machine
    { program = M.fromList $ zip [fromIntegral base, fromIntegral base + 4 ..] $ disassemble p
    , pc   = start
    , lr   = 0
    , ctr  = 0
    , gprs = replicate 32 0
    , xer  = 0
    , cr   = 0
    }
  where

  loop :: Machine -> IO ()
  loop m = when (not $ done m) $ do
    printf "program counter = 0x%08X\n" $ pc m
    loop $ step m

  done :: Machine -> Bool
  done m | pc m < base || pc m > base + (fromIntegral (BS.length p) + 4) = error $ printf "program counter out of range: 0x%08X" $ pc m
         | otherwise                                                     = pc m == base + (fromIntegral $ BS.length p)

step :: Machine -> Machine
step m = case program m M.! pc m of
  (_, InstructionUnknown opcd xo) -> error $ printf "unknown instruction:  address: 0x%08X  opcode: %d  extended opcode: %d" (pc m) opcd xo
  (i, Instruction _ _ a) -> foldl (act i) m (if null [ () | PC := _ <- a ] then a ++ [PC := Reg PC + 4] else a)

act :: Word32 -> Machine -> Action -> Machine
act i m (r := e) = case r of
  PC  -> m { pc  = e' }
  CR  -> m { cr  = fromIntegral e' }
  LR  -> m { lr  = e' }
  CTR -> m { ctr = e' }
  XER -> m { xer = e' }
  RA  -> m { gprs = replace (fromIntegral $ field 11 15) e' (gprs m) }
  RT  -> m { gprs = replace (fromIntegral $ field  6 10) e' (gprs m) }
  where
  e' = eval e

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
    If a b c -> if a /= 0 then eval b else eval c
    AA   -> field 30 30
    LI   -> (if bit  6 then 0xFFFFFFFFFC000000 else 0) .|. field 6 31 .&. complement 0x3
    LK   -> field 31 31
    RB   -> gprs m !! fromIntegral (field 16 20)
    RS   -> gprs m !! fromIntegral (field  6 10)
    SI   -> (if bit 16 then 0xFFFFFFFF00000000 else 0) .|. field 16 31
    SPR  -> shiftL (field 16 20) 5 .|. field 11 15
    UI   -> field 16 31
  
{-
    null [ () |  
  ADDI d GPR0 i -> set n d i
  ADDI d a    i -> set n d $ r a + i
  B aa lk i -> m { pc = if aa then pc m + i else i, lr = if lk then pc m + 4 else lr m }
  BC b c aa lk i -> mLink { pc = if branch then addr else pc mLink + 4 }
    where
    mDec = if elem b [DecThenOnZero, DecThenOnNotZero, DecThenOnZeroAndTrue, DecThenOnNotZeroAndTrue, DecThenOnZeroAndFalse, DecThenOnNotZeroAndFalse]
             then m { ctr = ctr m - 1 }
             else m
    mLink = mDec  { lr = if lk then pc mDec + 4 else lr mDec }
    addr  = if aa then i else pc mLink + i
    zero = ctr mDec == 0
    cond = testBit (cr m) (31 - c)
    branch = case b of
      Always                   -> True
      OnTrue                   ->                 cond
      OnFalse                  ->             not cond
      DecThenOnZero            ->     zero
      DecThenOnNotZero         -> not zero
      DecThenOnZeroAndTrue     ->     zero &&     cond
      DecThenOnNotZeroAndTrue  -> not zero &&     cond
      DecThenOnZeroAndFalse    ->     zero && not cond
      DecThenOnNotZeroAndFalse -> not zero && not cond

  MFMSR -> n
  MTMSR -> n
  MFSPR d CTR -> set n d $ ctr m
  MFSPR d LR  -> set n d $ lr  m
  MFSPR d XER -> set n d $ xer m
  MFSPR _ SRInvalid -> n
  MTSPR a CTR -> n { ctr = r a }
  MTSPR a LR  -> n { lr  = r a }
  MTSPR a XER -> n { xer = r a }
  MTSPR _ SRInvalid -> n
  OR  s a b False -> set n a $ r s .|. r b
  ORI s a i -> set n a $ r s .|. i
  Unknown a b c -> error $ printf "unknown instruction:  address: 0x%08X  instruction: 0x%08X  opcode: %d  extended opcode: %d" (pc m) a b c
  where
  n = m { pc = pc m + 4 }
  r :: GPR -> Int
  r a = gprs m !! gprIndex a

  set :: Machine -> GPR -> Int -> Machine
  set m r v = m { gprs = replace (gprIndex r) v (gprs m) }
-}

replace :: Int -> a -> [a] -> [a]
replace _ a [] = [a]
replace i a (b:c) | i <= 0    = a : c
                  | otherwise = b : replace (i - 1) a c

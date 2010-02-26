-- | PowerPC instruction set simulation.
module Language.PowerPC.Simulator
  ( Memory (..)
  , simulate
  ) where

import Control.Monad
import Data.Bits
import qualified Data.Map as M
import Data.Word
import Text.Printf

import Language.PowerPC.Opcode

-- | Memory interface.
data Memory = Memory
  { load  :: Word64 -> Int -> IO [Word8]
  , store :: Word64 -> [Word8] -> IO ()
  }

data Machine = Machine
  { pc      :: Word64
  , lr      :: Word64
  , ctr     :: Word64
  , gprs    :: [Word64]
  , xer     :: Word64
  , cr      :: Word32
  }

-- | Run simulation given memory interface, base address of program, starting address of program, and the program instructions.
simulate :: Memory -> Word64 -> Word64 -> [Word32] -> IO ()
simulate memory base start p = loop Machine
  { pc      = start
  , lr      = 0
  , ctr     = 0
  , gprs    = replicate 32 0
  , xer     = 0
  , cr      = 0
  }
  where

  program = M.fromList $ zip [fromIntegral base, fromIntegral base + 4 ..] p

  loop :: Machine -> IO ()
  loop m = when (not $ done m) $ do
    n <- step memory (program M.! pc m) m
    when (pc m + 4 /= pc n) $ printf "branched to: 0x%08X\n" $ pc n
    loop n

  done :: Machine -> Bool
  done m | pc m < base || pc m > base + (fromIntegral (length p) + 4) = error $ printf "program counter out of range: 0x%08X" $ pc m
         | otherwise                                                  = pc m == base + (fromIntegral $ length p)

step :: Memory -> Word32 -> Machine -> IO Machine
step memory instr m = case opcode instr of

  --[ I "add"      31 266 [ AddF [(Rc, CR 0), (OE, OV)] (Reg RA) RB 0 ]

  -- addi
  (14,   0) -> return $ set n rti $ if rai == 0 then si else si + ra

  -- addis
  (15,   0) -> return $ set n rti $ if rai == 0 then shiftL si 16 else shiftL si 16 + ra

  -- b
  (18,   0) -> return m { pc = li + if aa then 0 else pc m, lr = if lk then pc m + 4 else lr m }

  -- lwzu
  (33,   0) -> do
    a <- load memory ea 4
    return $ set (set n rti (fromBytes a)) rai ea
    where
    ea = ra + d

  -- mfmsr
  (31,  83) -> printf "warning (mfmsr): instruction ignored\n" >> return n

  -- mfspr
  (31, 339) -> case spr of
    0x01 -> return $ set n rti $ xer m
    0x08 -> return $ set n rti $ lr  m
    0x09 -> return $ set n rti $ ctr m
    _ -> printf "warning (mfspr): unknown code: 0x%X\n" spr >> return n

  -- mtmsr
  (31, 146) -> printf "warning (mtmsr): instruction ignored\n" >> return n

  -- mtspr
  (31, 467) -> case spr of
    0x01 -> return n { xer = rs }
    0x08 -> return n { lr  = rs }
    0x09 -> return n { ctr = rs }
    _ -> printf "warning (mtspr): unknown code: 0x%X\n" spr >> return n

  -- ori
  (24,   0) -> return $ set n rai $ rs .|. ui

  -- sth
  (44,   0) -> store memory ea (toBytes 2 rs) >> return n
    where
    ea = d + if rai == 0 then 0 else ra

  --, I "sthu"     45   0 [ EA := Reg RA + D, Store 2 RS, RA := Reg EA ]

  -- stw
  (36,   0) -> store memory ea (toBytes 4 rs) >> return n
    where
    ea = d + if rai == 0 then 0 else ra
  
  --, I "stwu"     37   0 [ EA := Reg RA + D, Store 4 RS, RA := Reg EA ]

  (a, b) -> error $ printf "unknown instruction:  address: 0x%08X  instruction: 0x%08X  opcd: %d  xo: %d" (pc m) instr a b

  where

  field :: Int -> Int -> Word64
  field h l = shiftR (fromIntegral instr) (31 - l) .&. foldl setBit 0 [0 .. l - h] 

  bit :: Int -> Bool
  bit n = testBit instr (31 - n)

  exts :: Int -> Word64 -> Word64
  exts n w = if testBit w (63 - n) then foldl setBit 0 [63 - n + 1 .. 63]  .|. w else w

  exts32 :: Int -> Word64 -> Word64
  exts32 n =  exts (32 + n)

  n = m { pc = pc m + 4 }

  set :: Machine -> Word64 -> Word64 -> Machine
  set m i v = m { gprs = replace i v $ gprs m }

  aa   = bit 30
  d    = exts32 16 $ field 16 31
  li   = exts32  6 $ field 6 31 .&. complement 0x3
  lk   = bit 31
  oe   = bit 21
  ra   = gprs m !! fromIntegral rai
  rai  = field 11 15
  rb   = gprs m !! fromIntegral rbi
  rbi  = field 16 20
  rc   = bit 31
  rs   = gprs m !! fromIntegral rsi
  rsi  = field 6 10
  rti  = field 6 10
  si   = exts32 16 $ field 16 31
  spr  = shiftL (field 16 20) 5 .|. field 11 15
  ui   = field 16 31







replace :: Integral b => b -> a -> [a] -> [a]
replace _ a [] = [a]
replace i a (b:c) | i <= 0    = a : c
                  | otherwise = b : replace (i - 1) a c

toBytes :: Int -> Word64 -> [Word8]
toBytes n _ | n <= 0 = []
toBytes n d = fromIntegral (shiftR d ((n - 1) * 8)) : toBytes (n - 1) d

fromBytes :: [Word8] -> Word64
fromBytes = f . reverse
  where
  f [] = 0
  f (a:b) = fromIntegral a .|. shiftL (f b) 8


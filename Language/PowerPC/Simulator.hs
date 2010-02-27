-- | PowerPC instruction set simulation.
module Language.PowerPC.Simulator
  ( Memory (..)
  , simulate
  ) where

import Control.Monad
import Data.Bits
import Data.Word
import Text.Printf

import Language.PowerPC.Opcode

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
step memory instr m = case opcode instr of

  -- add
  (31, 266) -> return n3
    where
    r = ra + rb
    ov = testBit ra 63 == testBit rb 63 && testBit ra 63 /= testBit r 63
    n1 = set n rti r
    n2 = if oe then (if ov then setOV n1 else clearOV n1) else n1
    n3 = if rc then cr0 n2 r else n2

  -- addi
  (14,   0) -> return $ set n rti $ if rai == 0 then si else si + ra

  -- addis
  (15,   0) -> return $ set n rti $ if rai == 0 then shiftL si 16 else shiftL si 16 + ra

  -- andi.
  (28,   0) -> return n2
    where
    r = rs .&. ui
    n1 = set n rai r
    n2 = cr0 n1 r

  -- b
  (18,   0) -> return m { pc = li + if aa then 0 else pc m, lr = if lk then pc m + 4 else lr m }

  -- bclr
  (19,  16) -> return m3
    where
    m1 = if not $ bo 2 then m { ctr = ctr m - 1 } else m
    ctr_ok = bo 2 || ((ctr m1 /= 0) /= bo 3)
    cond_ok = bo 0 || crbi m1 == bo 1
    m2 = if ctr_ok && cond_ok then m1 { pc = lr m1 .&. complement 3 } else m1 { pc = pc m1 + 4 }
    m3 = if lk then m2 { lr = pc m1 } else m2

  -- lbz
  (34,   0) -> do
    a <- load memory ea 1
    return $ set n rti $ fromBytes a
    where
    ea = d + if rai == 0 then 0 else ra

  -- lhw
  (40,   0) -> do
    a <- load memory ea 2
    return $ set n rti $ fromBytes a
    where
    ea = d + if rai == 0 then 0 else ra

  -- lwz
  (32,   0) -> do
    a <- load memory ea 4
    return $ set n rti $ fromBytes a
    where
    ea = d + if rai == 0 then 0 else ra

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

  -- sthu
  (45,   0) -> store memory ea (toBytes 2 rs) >> return (set n rai ea)
    where
    ea = ra + d

  -- stw
  (36,   0) -> store memory ea (toBytes 4 rs) >> return n
    where
    ea = d + if rai == 0 then 0 else ra
  
  -- stwu
  (37,   0) -> store memory ea (toBytes 4 rs) >> return (set n rai ea)
    where
    ea = ra + d

  (a, b) -> error $ printf "unknown instruction:  address: 0x%08X  instruction: 0x%08X  opcd: %d  xo: %d" (pc m) instr a b

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


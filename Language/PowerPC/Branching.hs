-- | PowerPC branch analysis.
module Language.PowerPC.Branching
  ( branching
  ) where

import Control.Monad
import Data.Bits
import Data.List
import Data.Word
import System.IO

import Language.PowerPC.Opcode

branching :: [Word32] -> Word64 -> [Word64] -> IO ()
branching program base starts = do

next :: (Word64, Word32) -> [Word64]
next (addr, instr) = case opcode instr of
  (18,   0) -> if aa then [li] else [addr + li] 
  (16,   0) -> (if branchAlways then [] else [addr + 4]) ++ (if aa then [bd] else [addr + bd])
  (19,  16) -> if branchAlways then [] else [addr + 4]
  (19, 528) -> if branchAlways then [] else [addr + 4]
  _ -> [addr + 4]

  where

  ufield :: Int -> Int -> Integer
  ufield h l = shiftR (fromIntegral instr) (31 - l) .&. setBits 0 [0 .. l - h]

  sfield :: Int -> Int -> Integer
  sfield h l = ufield h l .|. (if testBit instr (31 - h) then complement $ setBits 0 [0 .. l - h] else 0)

  aa = testBit 1 instr
  li = clearBits (sfield  6 31) [1, 0]
  bd = clearBits (sfield 16 31) [1, 0]
  bo = ufield  6 10
  branchAlways = testBit bo 4 && testBit bo 2


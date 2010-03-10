module Language.PowerPC.Utils
  ( setBits
  , clearBits
  ) where

import Data.Bits

clearBits :: Bits a => a -> [Int] -> a
clearBits = foldl clearBit

setBits :: Bits a => a -> [Int] -> a
setBits = foldl setBit


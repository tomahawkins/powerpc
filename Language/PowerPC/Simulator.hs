-- | General tools for PowerPC programs.
module Language.PowerPC.Simulator
  ( simulate
  ) where

import Control.Monad
import Data.Bits
import qualified Data.IntMap as M
import Text.Printf

import Language.PowerPC.Syntax

data Machine = Machine
  { program :: M.IntMap Instruction
  , pc      :: Int
  , lr      :: Int
  , ctr     :: Int
  , gprs    :: [Int]
  , xer     :: Int
  }

instance Show Machine where
  show m = printf "program counter = 0x%08X\n" $ pc m

simulate :: Int -> Int -> Program -> IO ()
simulate base start p = loop Machine
    { program = M.fromList $ zip [base, base + 4 ..] p
    , pc   = start
    , lr   = 0
    , ctr  = 0
    , gprs = replicate 32 0
    , xer  = 0
    }
  where

  loop :: Machine -> IO ()
  loop m = when (not $ done m) $ do
    printf "program counter = 0x%08X\n" $ pc m
    loop $ step m

  done :: Machine -> Bool
  done m | pc m < base || pc m > base + (length p + 1) * 4 = error $ printf "program counter out of range: 0x%08X" $ pc m
         | otherwise                                       = pc m == base + length p * 4

step :: Machine -> Machine
step m = case program m M.! pc m of
  B   a -> m { pc = pc m + a }
  BA  a -> m { pc = a }
  BL  a -> m { pc = pc m + a, lr = pc m + 4 }
  BLA a -> m { pc = a,        lr = pc m + 4 }
  MTSPR a CTR -> n { ctr = r a }
  MTSPR a LR  -> n { lr  = r a }
  MTSPR a XER -> n { xer = r a }
  Unknown a b c -> error $ printf "unknown instruction:  address: 0x%08X  instruction: 0x%08X  opcode: %d  extended opcode: %d" (pc m) a b c
  where
  n = m { pc = pc m + 4 }
  r :: GPR -> Int
  r a = gprs m !! gprIndex a

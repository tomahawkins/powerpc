-- | PowerPC assembly syntax.
module Language.PowerPC.Syntax
  ( Program
  , Instruction (..)
  , SR          (..)
  , GPR         (..)
  , gpr
  , gprIndex
  ) where

import Data.List
import Data.Maybe

type Program = [Instruction]

data Instruction
  = B   Int
  | BA  Int
  | BL  Int
  | BLA Int
  | MTSPR GPR SR
  | Unknown Int Int Int

data SR = CTR | LR | XER deriving (Show, Eq)

data GPR
  = GRP0
  | GRP1
  | GRP2
  | GRP3
  | GRP4
  | GRP5
  | GRP6
  | GRP7
  | GRP8
  | GRP9
  | GRP10
  | GRP11
  | GRP12
  | GRP13
  | GRP14
  | GRP15
  | GRP16
  | GRP17
  | GRP18
  | GRP19
  | GRP20
  | GRP21
  | GRP22
  | GRP23
  | GRP24
  | GRP25
  | GRP26
  | GRP27
  | GRP28
  | GRP29
  | GRP30
  | GRP31
  deriving (Show, Eq) 

gpr :: Int -> GPR
gpr a = if a < 0 || a > 31 then error "invalid GPR index" else
  [ GRP0
  , GRP1
  , GRP2
  , GRP3
  , GRP4
  , GRP5
  , GRP6
  , GRP7
  , GRP8
  , GRP9
  , GRP10
  , GRP11
  , GRP12
  , GRP13
  , GRP14
  , GRP15
  , GRP16
  , GRP17
  , GRP18
  , GRP19
  , GRP20
  , GRP21
  , GRP22
  , GRP23
  , GRP24
  , GRP25
  , GRP26
  , GRP27
  , GRP28
  , GRP29
  , GRP30
  , GRP31
  ] !! a

gprIndex :: GPR -> Int
gprIndex a = fromJust $ elemIndex a
  [ GRP0
  , GRP1
  , GRP2
  , GRP3
  , GRP4
  , GRP5
  , GRP6
  , GRP7
  , GRP8
  , GRP9
  , GRP10
  , GRP11
  , GRP12
  , GRP13
  , GRP14
  , GRP15
  , GRP16
  , GRP17
  , GRP18
  , GRP19
  , GRP20
  , GRP21
  , GRP22
  , GRP23
  , GRP24
  , GRP25
  , GRP26
  , GRP27
  , GRP28
  , GRP29
  , GRP30
  , GRP31
  ]




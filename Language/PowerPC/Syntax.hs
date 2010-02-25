-- | PowerPC assembly syntax.
module Language.PowerPC.Syntax
  ( Program
  , Instruction (..)
  , SR          (..)
  , GPR         (..)
  , gpr
  , gprIndex
  , BranchCondition (..)
  ) where

import Data.List
import Data.Maybe

type Program = [Instruction]

data Instruction
  = ADDI GPR GPR Int
  | B  Bool Bool Int
  | BC BranchCondition Int Bool Bool Int
  | MFMSR  --XXX
  | MTMSR  --XXX
  | MFSPR GPR SR
  | MTSPR GPR SR
  | OR  GPR GPR GPR Bool
  | ORI GPR GPR Int
  | Unknown Int Int Int
  deriving (Show, Eq)

data SR = CTR | LR | XER | SRInvalid deriving (Show, Eq)

data BranchCondition
  = Always
  | OnTrue
  | OnFalse
  | DecThenOnZero
  | DecThenOnNotZero
  | DecThenOnZeroAndTrue
  | DecThenOnNotZeroAndTrue
  | DecThenOnZeroAndFalse
  | DecThenOnNotZeroAndFalse
  deriving (Show, Eq)

data GPR
  = GPR0
  | GPR1
  | GPR2
  | GPR3
  | GPR4
  | GPR5
  | GPR6
  | GPR7
  | GPR8
  | GPR9
  | GPR10
  | GPR11
  | GPR12
  | GPR13
  | GPR14
  | GPR15
  | GPR16
  | GPR17
  | GPR18
  | GPR19
  | GPR20
  | GPR21
  | GPR22
  | GPR23
  | GPR24
  | GPR25
  | GPR26
  | GPR27
  | GPR28
  | GPR29
  | GPR30
  | GPR31
  deriving (Show, Eq) 

gpr :: Int -> GPR
gpr a = if a < 0 || a > 31 then error "invalid GPR index" else
  [ GPR0
  , GPR1
  , GPR2
  , GPR3
  , GPR4
  , GPR5
  , GPR6
  , GPR7
  , GPR8
  , GPR9
  , GPR10
  , GPR11
  , GPR12
  , GPR13
  , GPR14
  , GPR15
  , GPR16
  , GPR17
  , GPR18
  , GPR19
  , GPR20
  , GPR21
  , GPR22
  , GPR23
  , GPR24
  , GPR25
  , GPR26
  , GPR27
  , GPR28
  , GPR29
  , GPR30
  , GPR31
  ] !! a

gprIndex :: GPR -> Int
gprIndex a = fromJust $ elemIndex a
  [ GPR0
  , GPR1
  , GPR2
  , GPR3
  , GPR4
  , GPR5
  , GPR6
  , GPR7
  , GPR8
  , GPR9
  , GPR10
  , GPR11
  , GPR12
  , GPR13
  , GPR14
  , GPR15
  , GPR16
  , GPR17
  , GPR18
  , GPR19
  , GPR20
  , GPR21
  , GPR22
  , GPR23
  , GPR24
  , GPR25
  , GPR26
  , GPR27
  , GPR28
  , GPR29
  , GPR30
  , GPR31
  ]




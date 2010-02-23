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
  = ADDI GPR GPR Int
  | B   Int
  | BA  Int
  | BL  Int
  | BLA Int
  | MTSPR GPR SR
  | Unknown Int Int Int

data SR = CTR | LR | XER | SRInvalid deriving (Show, Eq)

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




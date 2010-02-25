module Language.PowerPC.Instructions.Addi (addi) where

import Language.PowerPC.Instruction

addi :: Instruction
addi = Instruction 14 0
  [ RT := If (Reg RA ==. 0) SI (SI + Reg RA)
  ]


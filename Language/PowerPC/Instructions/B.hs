module Language.PowerPC.Instructions.B (b) where

import Language.PowerPC.Instruction

b :: Instruction
b = Instruction 18 0
  [ PC := If AA LI (LI + Reg PC)
  , LR := If LK (Reg PC + 4) (Reg LR)
  ]


module Language.PowerPC.Instructions.Ori (ori) where

import Language.PowerPC.Instruction

ori :: Instruction
ori = Instruction 24 0
  [ RA := Or RS UI
  ]


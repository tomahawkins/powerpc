module Language.PowerPC.Instructions.Mfspr (mfspr) where

import Language.PowerPC.Instruction

mfspr :: Instruction
mfspr = Instruction 31 339
  [ RT := If (SPR ==. 1) (Reg XER) (If (SPR ==. 8) (Reg LR) (If (SPR ==. 9) (Reg CTR) (Reg RT)))
  ] --XXX Need to issue warning if case not covered.


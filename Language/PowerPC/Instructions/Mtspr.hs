module Language.PowerPC.Instructions.Mtspr (mtspr) where

import Language.PowerPC.Instruction

mtspr :: Instruction
mtspr = Instruction 31 467
  [ XER := If (SPR ==. 0x01) RS (Reg XER)
  , LR  := If (SPR ==. 0x08) RS (Reg LR )
  , CTR := If (SPR ==. 0x09) RS (Reg LR )
  ] --XXX Need to issue warning if case not covered.


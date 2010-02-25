-- | A list of all instructions.  File generated by SetupInstrs.hs
module Language.PowerPC.Instructions (instructions) where

import Language.PowerPC.Instruction
import Language.PowerPC.Instructions.Ori
import Language.PowerPC.Instructions.Addi
import Language.PowerPC.Instructions.Mfspr
import Language.PowerPC.Instructions.Mfmsr
import Language.PowerPC.Instructions.Mtmsr
import Language.PowerPC.Instructions.Mtspr
import Language.PowerPC.Instructions.B

-- | All available instructions.
instructions :: [Instruction]
instructions =
  [ ori
  , addi
  , mfspr
  , mfmsr
  , mtmsr
  , mtspr
  , b
  ]

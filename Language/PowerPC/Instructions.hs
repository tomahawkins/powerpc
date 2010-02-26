module Language.PowerPC.Instructions (instructions) where

import Data.Bits

import Language.PowerPC.Instruction

-- | All available instructions.
instructions :: [Instruction]
instructions =
  [ I "addi"     14   0 [ RT := If (RAI ==. 0) SI (SI + Reg RA) ]
  , I "addis"    15   0 [ RT := If (RAI ==. 0) (shiftL SI 16) (shiftL SI 16 + Reg RA) ]
  , I "b"        18   0 [ PC := If AA LI (LI + Reg PC), LR := If LK (Reg PC + 4) (Reg LR) ]
  , I "mfmsr"    31  83 [] --XXX
  , I "mfspr"    31 339 [ RT := If (SPR ==. 1) (Reg XER) (If (SPR ==. 8) (Reg LR) (If (SPR ==. 9) (Reg CTR) (Reg RT))) ]  --XXX Need to warn if SPR doesn't match.
  , I "mtmsr"    31 146 [] --XXX
  , I "mtspr"    31 467
    [ XER := If (SPR ==. 0x01) RS (Reg XER)
    , LR  := If (SPR ==. 0x08) RS (Reg LR )
    , CTR := If (SPR ==. 0x09) RS (Reg LR )
    ] --XXX Need to issue warning if case not covered.
  , I "ori"      24   0 [ RA := Or RS UI ]
  , I "sth"      44   0 [ Store2 (If (RAI ==. 0) 0 (Reg RA) + D) RS ]
  , I "sthu"     45   0 [ Store2 (Reg RA + D) RS, RA := Reg RA + D ]
  ]


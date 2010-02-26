module Language.PowerPC.Instructions (instructions) where

import Data.Bits

import Language.PowerPC.Instruction

-- | All available instructions.
instructions :: [Instruction]
instructions =
  [ I "addi"     14   0 [ RT := If (RAI ==. 0) SI (SI + Reg RA) ]
  , I "addis"    15   0 [ RT := If (RAI ==. 0) (shiftL SI 16) (shiftL SI 16 + Reg RA) ]
  , I "b"        18   0 [ PC := If AA LI (LI + Reg PC), LR := If LK (Reg PC + 4) (Reg LR) ]
  , I "lwzu"     33   0 [ EA := Reg RA + D, Load4, RA := Reg EA ]
  , I "mfmsr"    31  83 [] --XXX
  , I "mfspr"    31 339 [ RT := If (SPR ==. 1) (Reg XER) (If (SPR ==. 8) (Reg LR) (If (SPR ==. 9) (Reg CTR) (Reg RT))) ]  --XXX Need to warn if SPR doesn't match.
  , I "mtmsr"    31 146 [] --XXX
  , I "mtspr"    31 467
    [ XER := If (SPR ==. 0x01) RS (Reg XER)
    , LR  := If (SPR ==. 0x08) RS (Reg LR )
    , CTR := If (SPR ==. 0x09) RS (Reg LR )
    ] --XXX Need to issue warning if case not covered.
  , I "ori"      24   0 [ RA := Or RS UI ]
  , I "sth"      44   0 [ EA := If (RAI ==. 0) 0 (Reg RA) + D, Store2 RS ]
  , I "sthu"     45   0 [ EA := Reg RA + D, Store2 RS, RA := Reg EA ]
  , I "stw"      36   0 [ EA := If (RAI ==. 0) 0 (Reg RA) + D, Store4 RS ]
  , I "stwu"     37   0 [ EA := Reg RA + D, Store4 RS, RA := Reg EA ]
  ]


module Language.PowerPC.Instructions
  ( I (..)
  , instructions
  ) where

import Data.Bits

import Language.PowerPC.RTL

data I = I String Int Int (RTL ())

instructions :: [I]
instructions =
  [ I "add"    31 266 $ assign [When Rc $ CR' 0, When OE OV] RT $ RA + RB
  , I "addi"   14   0 $ if' (RAI ==. 0) (RT <== SI) (RT <== RA + SI)
  , I "addis"  15   0 $ if' (RAI ==. 0) (RT <== shiftL SI 16) (RT <== RA + shiftL SI 16)
  , I "andi"   28   0 $ assign [CR' 0] RA $ RS .&. UI
  , I "andis"  29   0 $ assign [CR' 0] RA $ RS .&. shiftL UI 16
  , I "b"      18   0 $ if' AA  (NIA <== LI) (NIA <== CIA + LI) >> if' LK (LR <== CIA + 4) (return ())
  , I "bc"     16   0 $ do
      -- XXX Assumes 64-bit mode.
      if' (Not $ Bit BO 2) (CTR <== CTR - 1) (return ())
      V "ctr_ok"  <== Bit BO 2 ||. ((CTR /=. 0) &&. Not (Bit BO 3) ||. (CTR ==. 0) &&. Bit BO 3)
      V "cond_ok" <== Bit BO 0 ||. (Bit CR BI ==. Bit BO 1)
      if' (V "ctr_ok" &&. V "cond_ok") (if' AA (NIA <== BD) (NIA <== CIA + BD)) (return ())
      if' LK (LR <== CIA + 4) (return ())
  , I "bclr"   19  16 $ do
      -- XXX Assumes 64-bit mode.
      if' (Not $ Bit BO 2) (CTR <== CTR - 1) (return ())
      V "ctr_ok"  <== Bit BO 2 ||. ((CTR /=. 0) &&. Not (Bit BO 3) ||. (CTR ==. 0) &&. Bit BO 3)
      V "cond_ok" <== Bit BO 0 ||. (Bit CR BI ==. Bit BO 1)
      if' (V "ctr_ok" &&. V "cond_ok") (NIA <== LR .&. complement 3) (return ())
      if' LK (LR <== CIA + 4) (return ())
  , I "lbz"    34   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "b" + D
      RT <== MEM (V "EA") 1
  , I "lbzx"   31  87 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "b" + RB
      RT <== MEM (V "EA") 1
  , I "lbzu"   35   0 $ do
      V "EA" <== RA + D
      RT <== MEM (V "EA") 1
      RA <== V "EA"
  , I "lbzux"  31 119 $ do
      V "EA" <== RA + RB
      RT <== MEM (V "EA") 1
      RA <== V "EA"
  , I "lhz"    40   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "B" + D
      RT <== MEM (V "EA") 2
  , I "lwz"    32   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "B" + D
      RT <== MEM (V "EA") 4
  , I "lwzu"   33   0 $ do
      V "EA" <== RA + D
      RT <== MEM (V "EA") 4
      RA <== V "EA"
  , I "mfmsr"  31  83 $ warning "mfmsr instruction ignored"
  , I "mfspr"  31 339 $ if' (SPR ==. 0x01) (RT <== XER)
                       (if' (SPR ==. 0x08) (RT <== LR)
                       (if' (SPR ==. 0x09) (RT <== CTR) (warning "unknown mfspr reg")))
  , I "mtmsr"  31 146 $ warning "mtmsr instruction ignored"
  , I "mtspr"  31 467 $ if' (SPR ==. 0x01) (XER <== RS)
                       (if' (SPR ==. 0x08) (LR  <== RS)
                       (if' (SPR ==. 0x09) (CTR <== RS) (warning "unknown mfspr reg")))
  , I "ori"    24   0 $ RA <== RS .|. UI
  , I "sth"    44   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "b" + D
      MEM (V "EA") 2 <== RS
  , I "sthx"   31 407 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "b" + RB
      MEM (V "EA") 2 <== RS
  , I "sthu"   45   0 $ do
      V "EA" <== RA + D
      MEM (V "EA") 2 <== RS
      RA <== V "EA"
  , I "sthux"  31 439 $ do
      V "EA" <== RA + RB
      MEM (V "EA") 2 <== RS
      RA <== V "EA"
  , I "stw"    36   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "b" + D
      MEM (V "EA") 4 <== RS
  , I "stwx"   31 151 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      V "EA" <== V "b" + RB
      MEM (V "EA") 4 <== RS
  , I "stwu"   37   0 $ do
      V "EA" <== RA + D
      MEM (V "EA") 4 <== RS
      RA <== V "EA"
  , I "stwux"  31 183 $ do
      V "EA" <== RA + RB
      MEM (V "EA") 4 <== RS
      RA <== V "EA"
  ] 


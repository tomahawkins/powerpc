module Language.PowerPC.Instructions
  ( rtl
  ) where

import Data.Bits
import Data.Word
import Text.Printf

import Language.PowerPC.Opcode
import Language.PowerPC.RTL

data I = I String Int Int (RTL ())

-- | Extracts the RTL statements of an instruction.
rtl :: Word64 -> Word32 -> (String, Stmt)
rtl addr instr = if null found
  then error $ printf "unknown instruction:  address: 0x%08X  instruction: 0x%08X  opcd: %d  xo: %d" addr instr opcd xo
  else if length found /= 1
    then error $ printf "overlapping opcode:  opcd: %d  xo: %d" opcd xo
    else head found
  where
  (opcd, xo) = opcode instr
  found = [ (n, snd $ f Null) | I n opcd' xo' (RTL f) <- instructions, opcd == opcd', xo == xo' ]

next :: I -> I
next (I name opcd xo rtl@(RTL f)) = I name opcd xo $ if branch s then rtl else rtl >> (NIA <== CIA + 4)
  where
  ((), s) = f Null
  branch :: Stmt -> Bool
  branch a = case a of
    Seq a b -> branch a || branch b
    Assign NIA _ -> True
    If _ m n | branch m == branch n -> branch m
             | otherwise -> error $ "NIA not assigned in all branches: " ++ show a
    _ -> False
  
instructions :: [I]
instructions = map next
  [ I "add"    31 266 $ RT <== Cond [When Rc $ CR' 0, When OE OV] (RA + RB)
  , I "addi"   14   0 $ if' (RAI ==. 0) (RT <== SI) (RT <== RA + SI)
  , I "addis"  15   0 $ if' (RAI ==. 0) (RT <== shiftL SI 16) (RT <== RA + shiftL SI 16)
  , I "andi"   28   0 $ RA <== Cond [CR' 0] (RS .&. UI)
  , I "andis"  29   0 $ RA <== Cond [CR' 0] (RS .&. shiftL UI 16)
  , I "b"      18   0 $ if' AA  (NIA <== LI) (NIA <== CIA + LI) >> if' LK (LR <== CIA + 4) (return ())
  , I "bc"     16   0 $ do
      -- XXX Assumes 64-bit mode.
      if' (Not $ Bit BO 5 2) (CTR <== CTR - 1) (return ())
      V "ctr_ok"  <== Bit BO 5 2 ||. ((CTR /=. 0) &&. Not (Bit BO 5 3) ||. (CTR ==. 0) &&. Bit BO 5 3)
      V "cond_ok" <== Bit BO 5 0 ||. (Bit CR 32 BI ==. Bit BO 5 1)
      if' (V "ctr_ok" &&. V "cond_ok") (if' AA (NIA <== BD) (NIA <== CIA + BD)) (NIA <== CIA + 4)
      if' LK (LR <== CIA + 4) (return ())
  , I "bclr"   19  16 $ do
      -- XXX Assumes 64-bit mode.
      if' (Not $ Bit BO 5 2) (CTR <== CTR - 1) (return ())
      V "ctr_ok"  <== Bit BO 5 2 ||. ((CTR /=. 0) &&. Not (Bit BO 5 3) ||. (CTR ==. 0) &&. Bit BO 5 3)
      V "cond_ok" <== Bit BO 5 0 ||. (Bit CR 32 BI ==. Bit BO 5 1)
      if' (V "ctr_ok" &&. V "cond_ok") (NIA <== LR .&. complement 3) (NIA <== CIA + 4)
      if' LK (LR <== CIA + 4) (return ())
  , I "lbz"    34   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      RT <== MEM EA 1
  , I "lbzx"   31  87 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + RB
      RT <== MEM EA 1
  , I "lbzu"   35   0 $ do
      EA <== RA + D
      RT <== MEM EA 1
      RA <== EA
  , I "lbzux"  31 119 $ do
      EA <== RA + RB
      RT <== MEM EA 1
      RA <== EA
  , I "lhz"    40   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      RT <== MEM EA 2
  , I "lwz"    32   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      RT <== MEM EA 4
  , I "lwzu"   33   0 $ do
      EA <== RA + D
      RT <== MEM EA 4
      RA <== EA
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
      EA <== V "b" + D
      MEM EA 2 <== RS
  , I "sthx"   31 407 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + RB
      MEM EA 2 <== RS
  , I "sthu"   45   0 $ do
      EA <== RA + D
      MEM EA 2 <== RS
      RA <== EA
  , I "sthux"  31 439 $ do
      EA <== RA + RB
      MEM EA 2 <== RS
      RA <== EA
  , I "stw"    36   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      MEM EA 4 <== RS
  , I "stwx"   31 151 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + RB
      MEM EA 4 <== RS
  , I "stwu"   37   0 $ do
      EA <== RA + D
      MEM EA 4 <== RS
      RA <== EA
  , I "stwux"  31 183 $ do
      EA <== RA + RB
      MEM EA 4 <== RS
      RA <== EA
  ] 


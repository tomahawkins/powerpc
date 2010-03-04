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
  found = [ (n, stmt f) | I n opcd' xo' f <- instructions, opcd == opcd', xo == xo' ]

next :: I -> I
next (I name opcd xo rtl) = I name opcd xo $ if branch (stmt rtl) then rtl else rtl >> (NIA <== CIA + 4)
  where
  branch :: Stmt -> Bool
  branch a = case a of
    Seq a -> or $ map branch a
    Assign _ NIA _ -> True
    If _ m n | branch m == branch n -> branch m
             | otherwise -> error $ "NIA not assigned in all branches: " ++ show a
    _ -> False
  
instructions :: [I]
instructions = map next $ concat [misc, arithmetic, logical, comparison, branch, rotating, shifting, load, store]

misc :: [I]
misc =
  [ I "mfmsr"  31  83 $ warning "mfmsr instruction ignored" []
  , I "mfspr"  31 339 $ if' (SPR ==. 0x01) (RT <== XER)
                       (if' (SPR ==. 0x08) (RT <== LR)
                       (if' (SPR ==. 0x09) (RT <== CTR) (warning "unknown mfspr register" [("SPR = %d", SPR)])))
  , I "mtmsr"  31 146 $ warning "mtmsr instruction ignored" []
  , I "mtspr"  31 467 $ if' (SPR ==. 0x01) (XER <== RS)
                       (if' (SPR ==. 0x08) (LR  <== RS)
                       (if' (SPR ==. 0x09) (CTR <== RS) (warning "unknown mtspr register" [("SPR = %d", SPR), ("RS = 0x%016X", RS)])))
  ]

arithmetic :: [I]
arithmetic =
  [ I "add"    31 266 $ assign [CR Rc 0, OV OE] RT (RA + RB)
  , I "addc"   31  10 $ assign [CA 1, CR Rc 0, OV OE] RT (RA + RB)
  , I "addi"   14   0 $ if' (RAI ==. 0) (RT <== SI) (RT <== RA + SI)
  , I "addic"  12   0 $ assign [CA 1] RT (RA + SI)
  , I "addic." 13   0 $ assign [CR 1 0, CA 1] RT (RA + SI)
  , I "addis"  15   0 $ if' (RAI ==. 0) (RT <== shiftL SI 16) (RT <== RA + shiftL SI 16)
  , I "subf"   31  40 $ assign [CR Rc 0, OV OE] RT (complement RA + RB + 1)
  , I "subfc"  31   8 $ assign [CA 1, CR Rc 0, OV OE] RT (complement RA + RB + 1)
  , I "subfic"  8   0 $ assign [CA 1] RT (complement RA + SI + 1)
  , I "mullw"  31 235 $ assign [CR Rc 0, OV OE] RT (EXTS 32 RA * EXTS 32 RB)
  ]

logical :: [I]
logical =
  [ I "and"    31  23 $ assign [CR Rc 0] RA (RS .&. RB)
  , I "andi"   28   0 $ assign [CR 1  0] RA (RS .&. UI)
  , I "andis"  29   0 $ assign [CR 1  0] RA (RS .&. shiftL UI 16)
  , I "or"     31 444 $ assign [CR Rc 0] RA (RS .|. RB)
  , I "ori"    24   0 $ RA <== RS .|. UI
  ]

comparison :: [I]
comparison =
  [ I "cmp"    31   0 $ do
      if' (L10 ==. 0)
        (do V "a" <== EXTS 32 RA
            V "b" <== EXTS 32 RB)
        (do V "a" <== RA
            V "b" <== RB)
      cmp [CR 1 BF] (V "a") (V "b")
  , I "cmpi"   11   0 $ do
      if' (L10 ==. 0) (V "a" <== EXTS 32 RA) (V "a" <== RA)
      cmp [CR 1 BF] (V "a") SI
  , I "cmpl"   31  32 $ do
      if' (L10 ==. 0)
        (do V "a" <== 0xFFFFFFFF .|. RA
            V "b" <== 0xFFFFFFFF .|. RB)
        (do V "a" <== RA
            V "b" <== RB)
      cmp [CR 1 BF] (V "a") (V "b")
  , I "cmpli"  10   0 $ do
      if' (L10 ==. 0) (V "a" <== 0xFFFFFFFF .|. RA) (V "a" <== RA)
      cmp [CR 1 BF] (V "a") UI
  ]

branch :: [I]
branch =
  [ I "b"      18   0 $ if' AA  (NIA <== LI) (NIA <== CIA + LI) >> if' LK (LR <== CIA + 4) (return ())
  , I "bc"     16   0 $ do
      -- XXX Assumes 64-bit mode.
      if' (Not $ Bit BO 5 2) (CTR <== CTR - 1) (return ())
      V "ctr_ok"  <== Bit BO 5 2 ||. ((CTR /=. 0) &&. Not (Bit BO 5 3) ||. (CTR ==. 0) &&. Bit BO 5 3)
      V "cond_ok" <== Bit BO 5 0 ||. (Bit CReg 32 BI ==. Bit BO 5 1)
      if' (V "ctr_ok" &&. V "cond_ok") (if' AA (NIA <== BD) (NIA <== CIA + BD)) (NIA <== CIA + 4)
      if' LK (LR <== CIA + 4) (return ())
  , I "bclr"   19  16 $ do
      -- XXX Assumes 64-bit mode.
      if' (Not $ Bit BO 5 2) (CTR <== CTR - 1) (return ())
      V "ctr_ok"  <== Bit BO 5 2 ||. ((CTR /=. 0) &&. Not (Bit BO 5 3) ||. (CTR ==. 0) &&. Bit BO 5 3)
      V "cond_ok" <== Bit BO 5 0 ||. (Bit CReg 32 BI ==. Bit BO 5 1)
      if' (V "ctr_ok" &&. V "cond_ok") (NIA <== LR .&. complement 3) (NIA <== CIA + 4)
      if' LK (LR <== CIA + 4) (return ())
  ]

rotating :: [I]
rotating =
  [ I "rlwimi" 20 0 $ do
      V "n" <== SH5
      V "r" <== ROTL32 RS (V "n")
      V "m" <== MASK (MB5 + 32) (ME5 + 32)
      RA    <== V "r" .&. V "m" .|. RA .&. complement (V "m")
  , I "rlwinm" 21 0 $ do
      V "n" <== SH5
      V "r" <== ROTL32 RS (V "n")
      V "m" <== MASK (MB5 + 32) (ME5 + 32)
      RA    <== V "r" .&. V "m"
  ]

shifting :: [I]
shifting =
  [
  ]

load :: [I]
load =
  [ I "lbz"    34   0 $ do
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
  , I "lmw"    46   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      V "r" <== RTI
      while (V "r" <=. 31) $ do
        GPR (V "r") <== MEM EA 4
        V "r" <== V "r" + 1
        EA <== EA + 4
  , I "lwz"    32   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      RT <== MEM EA 4
  , I "lwzu"   33   0 $ do
      EA <== RA + D
      RT <== MEM EA 4
      RA <== EA
  ]

store :: [I]
store =
  [ I "stb"    38   0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      MEM EA 1 <== RS
  , I "stbx"   31 215 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + RB
      MEM EA 1 <== RS
  , I "stbu"   39   0 $ do
      EA <== RA + D
      MEM EA 1 <== RS
      RA <== EA
  , I "stbux"  31 247 $ do
      EA <== RA + RB
      MEM EA 1 <== RS
      RA <== EA
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
  , I "stmw" 47 0 $ do
      if' (RAI ==. 0) (V "b" <== 0) (V "b" <== RA)
      EA <== V "b" + D
      V "r" <== RSI
      while (V "r" <=. 31) $ do
        MEM EA 4 <== GPR (V "r")
        V "r" <== V "r" + 1
        EA <== EA + 4
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


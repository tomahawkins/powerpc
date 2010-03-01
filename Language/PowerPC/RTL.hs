module Language.PowerPC.RTL
  ( RTL  (..)
  , Stmt (..)
  , E    (..)
  , Cond (..)
  , (<==)
  , (==.)
  , (/=.)
  , (&&.)
  , (||.)
  , assign
  , if'
  , warning
  ) where

import Data.Bits
import Data.Word

--infix  4 ==., /=., <., <=., >., >=.
--infixl 3 &&.
--infixl 2 ||.
infixr 1 <==

data RTL a = RTL (Stmt -> (a, Stmt))

instance Monad RTL where
  return a = RTL (\ s -> (a, s))
  (RTL f1) >>= f2 = RTL f3
    where
    f3 s0 = f4 s1
      where
      (a, s1) = f1 s0
      RTL f4 = f2 a

data E
  = V String
  | C Word64
  | Add E E
  | Sub E E
  | Not E
  | And E E
  | Or  E E
  | BWNot E
  | BWAnd E E
  | BWOr  E E
  | Shift E E
  | Eq E E
  | Bit E E
  | AA
  | BD
  | BI
  | BO
  | CIA
  | CR
  | CTR
  | D
  | LI
  | LK
  | LR
  | MEM E Int
  | NIA
  | OE
  | RAI
  | RA
  | RB
  | Rc
  | RS
  | RT
  | SI
  | SPR
  | UI
  | XER
  deriving (Show, Eq)

data Stmt
  = Concat Stmt Stmt
  | Null
  | Assign E E
  | AssignCond [Cond] E E
  | If E Stmt Stmt
  | Warning String
  deriving (Show, Eq)

data Cond
  = CR' E
  | OV
  | CA
  | When E Cond
  deriving (Show, Eq)

(<==) :: E -> E -> RTL ()
a <== b = RTL $ \ s0 -> ((), Concat s0 $ Assign a b)

assign :: [Cond] -> E -> E -> RTL ()
assign conds a b = RTL $ \ s0 -> ((), Concat s0 $ AssignCond conds a b)

warning :: String -> RTL ()
warning msg = RTL $ \ s -> ((), Concat s $ Warning msg)

if' :: E -> RTL () -> RTL () -> RTL ()
if' a (RTL b) (RTL c) = RTL $ \ s0 -> ((), Concat s0 $ If a sb sc)
  where
  ((), sb) = b Null
  ((), sc) = c Null

(==.) :: E -> E -> E
(==.) = Eq

(/=.) :: E -> E -> E
(/=.) a b = Not $ (==.) a b

(&&.) :: E -> E -> E
(&&.) = And

(||.) :: E -> E -> E
(||.) = Or

instance Num E where
  a + b = Add a b
  a - b = Sub a b
  --a * b = Mul a b
  (*) = undefined
  negate a = 0 - a
  --abs a = mux (a <. 0) (negate a) a
  abs = undefined
  --signum a = mux (a ==. 0) 0 $ mux (a <. 0) (-1) 1
  signum = undefined
  fromInteger = C . fromInteger

instance Bits E where
  a .&. b = BWAnd a b
  complement a = BWNot a
  a .|. b = BWOr a b
  xor a b = (a .&. complement b) .|. (complement a .&. b)
  shift a n = Shift a $ C $ fromIntegral n
  rotate = undefined
  bitSize = undefined
  isSigned = undefined


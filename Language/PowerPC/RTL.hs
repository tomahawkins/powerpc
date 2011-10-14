module Language.PowerPC.RTL
  ( RTL  (..)
  , Stmt (..)
  , E    (..)
  , Cond (..)
  , stmt
  , (<==)
  , assign
  , cmp
  , if'
  , while
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , (&&.)
  , (||.)
  ) where

import Data.Bits

infix  4 ==., /=., <., <=., >., >=.
infixl 3 &&.
infixl 2 ||.
infixr 0 <==

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
  | C Integer
  | Add E E
  | Sub E E
  | Mul E E
  | Div E E
  | Not E
  | And E E
  | Or  E E
  | BWNot E
  | BWAnd E E
  | BWOr  E E
  | Shift E E
  | Eq E E
  | Lt E E
  | Null
  | Bit E Int E
  | AA
  | BA
  | BB
  | BD
  | BF
  | BI
  | BO
  | BT
  | CA'
  | CIA
  | CR'
  | CRField E
  | CTR
  | D
  | EA
  | EXTS Int E
  | GPR E
  | L10
  | L15
  | LI
  | LK
  | LR
  | MASK E E    -- ^ Mask start stop
  | MB5
  | MB6
  | ME5
  | ME6
  | MEM E Int
  | MSR
  | NIA
  | OE
  | RAI
  | RA
  | RB
  | Rc
  | ROTL32 E E  -- ^ ROTL32 value amount
  | ROTL64 E E  -- ^ ROTL64 value amount
  | RS
  | RSI
  | RT
  | RTI
  | SH5
  | SH6
  | SI
  | SPR
  | UI
  | XER
  deriving (Show, Eq, Ord)

data Stmt
  = Seq [Stmt]
  | Assign  [Cond] E E
  | If E Stmt Stmt
  | While E Stmt
  deriving (Show, Eq)

data Cond
  = CA E
  | OV E
  | CR E E
  deriving (Show, Eq, Ord)

stmt :: RTL () -> Stmt
stmt (RTL f) = snd $ f $ Seq []

(<==) :: E -> E -> RTL ()
a <== b = assign [] a b

assign :: [Cond] -> E -> E -> RTL ()
assign cond a b = RTL $ \ s0 -> ((), seqStmts s0 $ Assign cond a b)

cmp :: [Cond] -> E -> E -> RTL ()
cmp cond a b = assign cond Null $ a - b

if' :: E -> RTL () -> RTL () -> RTL ()
if' a b c = RTL $ \ s0 -> ((), seqStmts s0 $ If a (stmt b) (stmt c))

while :: E -> RTL () -> RTL ()
while a b = RTL $ \ s0 -> ((), seqStmts s0 $ While a (stmt b))

seqStmts :: Stmt -> Stmt -> Stmt
seqStmts (Seq []) a      = a
seqStmts a (Seq [])      = a
seqStmts (Seq a) (Seq b) = Seq $ a ++ b
seqStmts (Seq a) b       = Seq $ a ++ [b]
seqStmts a (Seq b)       = Seq $ a : b
seqStmts a b             = Seq [a, b]

(==.) :: E -> E -> E
(==.) = Eq

(/=.) :: E -> E -> E
(/=.) a b = Not $ (==.) a b

-- | Less than.
(<.) :: E -> E -> E
(<.) = Lt

-- | Greater than.
(>.) :: E -> E -> E
a >. b = b <. a

-- | Less than or equal.
(<=.) :: E -> E -> E
a <=. b =  Not (a >. b)

-- | Greater than or equal.
(>=.) :: E -> E -> E
a >=. b = Not (a <. b)

(&&.) :: E -> E -> E
(&&.) = And

(||.) :: E -> E -> E
(||.) = Or

instance Num E where
  a + b = Add a b
  a - b = Sub a b
  a * b = Mul a b
  negate a = 0 - a
  --abs a = mux (a <. 0) (negate a) a
  abs = error "abs"
  --signum a = mux (a ==. 0) 0 $ mux (a <. 0) (-1) 1
  signum = error "signum"
  fromInteger = C . fromInteger

instance Bits E where
  a .&. b = BWAnd a b
  complement a = BWNot a
  a .|. b = BWOr a b
  xor a b = (a .&. complement b) .|. (complement a .&. b)
  shift a n = Shift a $ C $ fromIntegral n
  rotate = error "rotate"
  bitSize = error "bitSize"
  isSigned = error "isSigned"


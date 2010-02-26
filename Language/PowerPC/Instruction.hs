module Language.PowerPC.Instruction
  ( Instruction (..)
  , Action (..)
  , R (..)
  , E (..)
  -- , not_
  -- , (&&.)
  -- , (||.)
  , (==.)
  -- , (/=.)
  , (<.)
  -- , (<=.)
  -- , (>.)
  -- , (>=.)
  , Flags (..)
  ) where

import Data.Bits
import Data.Word

--infix  4 ==., /=., <., <=., >., >=.
--infixl 3 &&.
--infixl 2 ||.
infixr 1 :=

data Instruction
  = I
    { mnemonic :: String
    , opcd     :: Int
    , xo       :: Int
    , action   :: [Action]
    }
  | IUnknown
    { opcd     :: Int
    , xo       :: Int
    }

data Action
  = R := E
  | Load Int
  | Store Int E
  | AddF [(E, Flags)] E E E


data R
  = PC    -- ^ Program Counter
  -- | CR    -- ^ Condition Register
  | LR    -- ^ Link Register
  | CTR   -- ^ Count Register
  | XER   -- ^ Fixed-Point Exception Register
  -- | FPSCR -- ^ Floating-Point Status and Control Register
  | RA
  | RT
  | EA
  deriving (Show, Eq)

data E
  -- General expressions.
  = Const Word64
  | Reg R
  | Add E E
  | Sub E E
  | Mul E E
  | Not E
  | And E E
  | Or  E E
  | Eq  E E
  | Lt  E E
  | Shift E Int
  | If E E E
  | Exts Int E
  -- Bit fields and registers.
  | AA
  | D
  | LI
  | LK
  | OE
  | RAI
  | RB
  | Rc
  | RS
  | SI
  | SPR
  | UI
  deriving (Show, Eq)

instance Num E where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate a = 0 - a
  abs a = If (a <. 0) (negate a) a
  signum a = If (a ==. 0) 0 $ If (a <. 0) (-1) 1
  fromInteger = Const . fromInteger

instance Bits E where
  complement = Not
  (.&.)      = And
  (.|.)      = Or
  xor a b = (a .&. complement b) .|. (complement a .&. b)
  shift = Shift
  rotate = undefined
  bitSize = undefined
  isSigned = undefined


{-
instance (OrdE a, NumE a, Num a, Fractional a) => Fractional (E a) where
  (Const a) / (Const b) = Const $ a / b
  a / b = Div a b
  recip a = 1 / a
  fromRational r = Const $ fromInteger (numerator r) / fromInteger (denominator r)

instance (Num a, Fractional a, Floating a, FloatingE a) => Floating (E a) where
  pi       = Const pi
  exp      (Const a) = Const $ exp a
  exp      a         = Exp a
  log      (Const a) = Const $ log a
  log      a         = Log a
  sqrt     (Const a) = Const $ sqrt a
  sqrt     a         = Sqrt a
  (**)     (Const a) (Const b) = Const $ a ** b
  (**)     a b       = Pow a b
  sin      (Const a) = Const $ sin a
  sin      a         = Sin a
  cos      a         = sqrt (1 - sin a ** 2)
  sinh     a         = (exp a - exp (-a)) / 2
  cosh     a         = (exp a + exp (-a)) / 2
  asin     (Const a) = Const $ asin a
  asin     a         = Asin a
  acos     a         = pi / 2 - asin a
  atan     a         = asin (a / (sqrt (a ** 2 + 1)))
  asinh    a         = log (a + sqrt (a ** 2 + 1))
  acosh    a         = log (a + sqrt (a ** 2 - 1))
  atanh    a         = 0.5 * log ((1 + a) / (1 - a))
-}

{-
-- | True term.
true :: E
true = Const 1

-- | False term.
false :: E
false = Const 0

-- | Logical negation.
not_ :: E Bool -> E Bool
not_ = Not

-- | Logical AND.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = And

-- | Logical OR.
(||.) :: E Bool -> E Bool -> E Bool
(||.) a b = not_ $ not_ a &&. not_ b
-}

-- | Equal.
(==.) :: E -> E -> E
(==.) = Eq

-- | Not equal.
--(/=.) :: EqE a => E a -> E a -> E Bool
--a /=. b = not_ (a ==. b)

-- | Less than.
(<.) :: E -> E -> E
(<.) = Lt

{-
-- | Greater than.
(>.) :: OrdE a => E a -> E a -> E Bool
a >. b = b <. a

-- | Less than or equal.
(<=.) :: OrdE a => E a -> E a -> E Bool
a <=. b =  not_ (a >. b)

-- | Greater than or equal.
(>=.) :: OrdE a => E a -> E a -> E Bool
a >=. b = not_ (a <. b)
-}

data Flags
  = CR E
  | OV
  | CA
  deriving (Show, Eq)

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax where

import           Data.Text (Text,unpack)
import           Data.Text.Lazy(pack)
import           Data.Hashable
import           Data.Label
import           Data.String
import           Data.Lens (Prism')
import qualified Data.Lens as L
import           Data.GraphViz.Attributes

import Control.Monad.State

-- Literals of Scheme
data Literal
  = Number Int
  | Float Double
  | Ratio Rational
  | Bool Bool
  | Char Char --single character and single quotation (')
  | String String -- any amount of chars and double quotation (")
  | Symbol String
  | Quote Literal
  -- | DottedList [Literal] Literal
  deriving (Eq)

data Op1_
-- | Numerical operations
  = Number_ -- (number? z)
  | Integer_ -- (integer? z)
  | Float_ -- (float? z)
  | Ratio_ -- (rational? z)
  | Zero -- (zero? z)
  | Positive -- (positive? z)
  | Negative -- (negative? z)
  | Odd -- (odd? z)
  | Even-- (even? z)
  | Abs -- (abs z)
  | Floor -- (floor z)
  | Ceiling -- (ceiling z)
  | Log -- (log z) base e
-- | Boolean operations
  | Boolean -- (boolean? z)
  | Not -- (not z)
-- | List operations
  | Null -- (null? z)
  | ListS -- (list? z)
  | Car -- (car z)
  | Cdr -- (cdr z)
  | Caar -- (caar z)
  | Cadr -- (cadr z)
  | Cddr -- (cddr z)
  | Caddr -- (caddr z)
  | Error -- (error z)
  deriving (Eq)

data Op2_
-- | Equivalence predicates
  = Eqv -- (eq? z) / (eqv? z)
  | Equal -- (equal? z)
-- | Numerical operations
  | Quotient -- (quotient z1 z2)
  | Remainder -- (remainder z1 z2)
  | Modulo -- (modulo z1 z2)
-- | List operations
  deriving (Eq)

data OpVar_
-- | Numerical operations
  = EqualS -- (= z1 z2 z3 ...)
  | SmallerS -- (< z1 z2 z3 ...)
  | GreaterS -- (> z1 z2 z3 ...)
  | SmallerEqualS -- (<= z1 z2 z3 ...)
  | GreaterEqualS -- (>= z1 z2 z3 ...)
  | Max -- (max z1 z2 z3 ...)
  | Min -- (max z1 z2 z3 ...)
  | Add -- (+) (+ z) (+ z1 z2 z3 ...)
  | Mul -- (*) (* z) (* z1 z2 z3 ...)
  | Sub -- (- z) (- z1 z2 z3 ...)
  | Div -- (/ z) (/ z1 z2 z3 ...)
  | Gcd -- (gcd z1 ...)
  | Lcm -- (lcm z1 ...)
-- | Boolean operations
  | And -- (and z1 z2 z3 ...)
  | Or -- (or z1 z2 z3 ...)
-- | List operations
  deriving (Eq)


-- | Expressions of Scheme. Each expression has a label, with which the
-- expression can be uniquely identified.
data Expr
-- | Expressions for inner representation and evaluation
  = Lit Literal Label
  | List [Expr] Label -- '(1 2 3)
  | Cons Expr Expr Label -- (cons z1 z2)

  -- | List_ [Expr] Label -- (list z1 z2 z3 ...)
  | Begin [Expr] Label
  | App Expr [Expr] Label
  | Apply [Expr] Label
-- | Scheme expressions
  | Var Text Label
  | Set Text Expr Label
  | Define Text Expr Label
  | Lam [Text] [Expr] Label
  | If Expr Expr Expr Label
  | Let [(Text, Expr)] [Expr] Label
  | LetRec [(Text, Expr)] [Expr] Label
-- | Scheme standard procedures
  | Op1 Op1_ Expr Label
  | Op2 Op2_ Expr Expr Label
  | OpVar OpVar_ [Expr] Label
  deriving (Eq)


-- Smart constructors that build labeled Scheme expressions.
-- | Expressions for inner representation and evaluation
lit :: Literal -> State Label Expr
lit x = Lit x <$> fresh
list :: [State Label Expr] -> State Label Expr 
list xs = List <$> (sequence xs) <*> fresh 
-- list_ :: [State Label Expr] -> State Label Expr 
-- list_ xs = List_ <$> (sequence xs) <*> fresh 
cons :: State Label Expr -> State Label Expr ->State Label Expr 
cons e1 e2 = Cons <$> e1 <*> e2 <*> fresh
begin :: [State Label Expr] -> State Label Expr
begin es = Begin <$> (sequence es) <*> fresh
app :: State Label Expr -> [State Label Expr] -> State Label Expr
app e1 e2 = App <$> e1 <*> (sequence e2) <*> fresh
-- | Scheme expressions
var_ :: Text -> State Label Expr
var_ x = Var x <$> fresh
set :: Text -> State Label Expr -> State Label Expr
set t e = Set t <$> e <*> fresh
lam :: [Text] -> [State Label Expr] -> State Label Expr
lam xs es = Lam xs <$> (sequence es) <*> fresh
if_ :: State Label Expr -> State Label Expr -> State Label Expr -> State Label Expr
if_ e1 e2 e3 = If <$> e1 <*> e2 <*> e3 <*> fresh
define :: Text -> State Label Expr -> State Label Expr
define t e = Define t <$> e <*> fresh
let_ :: [(Text, State Label Expr)] -> [State Label Expr] -> State Label Expr
let_ bnds body = Let <$> sequence [(v,) <$> e | (v,e) <- bnds] <*> (sequence body) <*> fresh
let_rec :: [(Text, State Label Expr)] -> [State Label Expr] -> State Label Expr
let_rec bnds body = LetRec <$> sequence [(v,) <$> e | (v,e) <- bnds] <*> (sequence body) <*> fresh
-- | Scheme standard procedures
op1_ :: Op1_ -> State Label Expr -> State Label Expr
op1_ operation e1 = Op1 operation <$> e1 <*> fresh
op2_ :: Op2_ -> State Label Expr -> State Label Expr -> State Label Expr
op2_ operation e1 e2 = Op2 operation <$> e1 <*> e2 <*> fresh
opvar_ :: OpVar_ -> [State Label Expr] -> State Label Expr
opvar_ operation es = OpVar operation <$> (sequence es) <*> fresh

-- instance Eq Expr where 
--   (==) e1 e2 = case (e1,e2) of 
--     (Lit _ l1, Lit _ l2) ->  l1 == l2 
--     (Cons _ _ l1, Cons _ _ l2) -> l1 == l2 
--     (List _ l1, List _ l2) -> l1 == l2
--     (Begin _ l1, Begin _ l2) -> l1 == l2 
--     (App _ _ l1, App _ _ l2) -> l1 == l2 
--     (Apply _ l1, Apply _ l2) -> l1 == l2 
--     (Var _ l1, Var _ l2) -> l1 == l2 
--     (Set _ _ l1, Set _ _ l2) -> l1 == l2
--     (Define _ _ l1, Define _ _ l2) -> l1 == l2
--     (Lam _ _ l1, Lam _ _ l2) -> l1 == l2 
--     (If _ _ _ l1, If _ _ _ l2) -> l1 == l2 
--     (Let _ _ l1, Let _ _ l2) -> l1 == l2 
--     (LetRec _ _ l1, LetRec _ _ l2) -> l1 == l2
--     (Op1 _ _ l1, Op1 _ _ l2) -> l1 == l2 
--     (Op2 _ _ _ l1, Op2 _ _ _ l2) -> l1 == l2
--     (OpVar _ _ l1, OpVar _ _ l2) -> l1 == l2 
--     _ -> False 

instance Show Literal where
  showsPrec _ e0 = case e0 of
    Number x -> showString "Number " . shows x
    Float x -> showString "Float " . shows x
    Ratio x -> showString "Ratio " . shows x
    Bool x -> showString "Bool " . shows x
    Char x -> showString "Char " .shows x --single character and single quotation (')
    String x -> showString "String " . shows x  -- any amount of chars and double quotation (")
    Symbol x -> showString "Symbol " . shows x
    Quote x -> showString "Quote " . shows x
    -- DottedList xs x -> showString ("DottedList ") . showList(xs) . showString (" . ") . shows (x)

instance Show Op1_ where
  showsPrec _ e0 = case e0 of
    Number_ -> showString "number? "
    Integer_ -> showString "integer? "
    Float_ -> showString "float? "
    Ratio_ -> showString "rational? "
    Zero -> showString "zero? "
    Positive -> showString "positive? "
    Negative -> showString "negative? "
    Odd -> showString "odd? "
    Even -> showString "even? "
    Abs -> showString "abs "
    Floor -> showString "floor "
    Ceiling -> showString "ceiling "
    Log -> showString "log "
    Boolean -> showString "boolean? "
    Not -> showString "not "
    Null -> showString "null? "
    ListS -> showString "list? "
    Car -> showString "car "
    Cdr -> showString "cdr "
    Caar -> showString "caar "
    Cadr -> showString "cadr "
    Cddr -> showString "cddr"
    Caddr -> showString "caddr"
    Error -> showString "error"

instance Show Op2_ where
  showsPrec _ e0 = case e0 of
    Eqv -> showString "eq? "
    Equal -> showString "equal? "
    Quotient -> showString "quotient "
    Remainder -> showString "remainder "
    Modulo -> showString "modulo "

instance Show OpVar_ where
  showsPrec _ e0 = case e0 of
    EqualS -> showString "= "
    SmallerS -> showString "< "
    GreaterS -> showString "> "
    SmallerEqualS -> showString "<= "
    GreaterEqualS -> showString ">= "
    Max -> showString "max "
    Min -> showString "min "
    Add -> showString "+ "
    Mul -> showString "* "
    Sub -> showString "- "
    Div -> showString "/ "
    Gcd -> showString "gcd "
    Lcm -> showString "lcm "
    And -> showString "and "
    Or -> showString "or "
    -- List_ -> showString ("list")

instance Show Expr where
  showsPrec d e0 = case e0 of
    Lit x _ -> shows x
    List xs _ -> showString "List " . showList xs 
      
    Cons e1 e2 _ -> showString "cons " . showsPrec (app_prec + 1) e1 . showsPrec (app_prec + 1) e2
    Begin es _->
      showString "{"
      . shows es
      . showString "}"
    App e1 e2 _ -> showParen (d > app_prec)
      $ showsPrec (app_prec + 1) e1
      . showString " "
      . showsPrec (app_prec + 1) e2
    Apply e _ -> showParen (d > app_prec) $ showsPrec (app_prec + 1) e
    Var x _ -> showString (unpack x)
    Set t e _ -> showParen (d > app_prec)
      $ showString "set"
      . showString (unpack t)
      . showString " "
      . shows e
    Define t e _ -> showParen (d > app_prec)
      $ showString "define "
      . showString (unpack t)
      . showString " "
      . shows e
    Lam xs e2 _ -> showParen (d > lam_prec)
      $ showString "lam"
      . showList (map unpack xs)
      . showString ". "
      . shows e2
    If e1 e2 e3 _ -> showParen (d > app_prec)
      $ showString "If "
      . showsPrec (app_prec + 1) e1
      . showString " "
      . showsPrec (app_prec + 1) e2
      . showString " "
      . showsPrec (app_prec + 1) e3
    Let bnds body _ -> showParen (d > let_prec)
      $ showString "let "
      . shows bnds
      . showString " in "
      . showsPrec (app_prec + 1) body
    LetRec bnds body _ -> showParen (d > let_prec)
      $ showString "letrec "
      . shows bnds
      . showString " in "
      . showsPrec (app_prec + 1) body
    Op1 op1 e _ -> showParen (d > app_prec)
      $ shows op1
      . shows e
    Op2 op2 e1 e2 _ -> showParen (d > app_prec)
      $ shows op2
      . shows e1
      . showString " "
      . shows e2
    OpVar opvar es _ -> showParen (d > app_prec)
      $ shows opvar
      . shows es
-- TODO : add op1 op2 opvar
    where
      app_prec = 10
      lam_prec = 9
      let_prec = 8

showTopLvl :: Expr -> String 
showTopLvl e = case e of 
    Lit x _ -> "Lit " ++ show x
    List _ _ -> "List "
    Cons _ _ _ -> "Cons"
    Begin _ _-> "Begin" 
    App _ _ _ -> "App"
    Apply _ _ -> "Apply"
    Var x _ -> "Var " ++ unpack x
    Set t _ _ -> "Set " ++ unpack t ++ " := " ++ showTopLvl e  
    Define t _ _ -> "Define " ++ unpack t ++ " := " ++ showTopLvl e  
    Lam xs e2 _ -> "Lam " ++ unwords (map unpack xs) ++ " -> " ++ unwords (map showTopLvl e2)
    If e1 _ _ _ -> "If(" ++ showTopLvl e1 ++ ")"
    Let _ _ _ -> "Let"
    LetRec _ _ _ -> "LetRec"
    Op1 op1 _ _ -> show op1 ++ " (" ++ showTopLvl e ++ ")"
    Op2 op2 e1 e2 _ -> show op2 ++ " (" ++  showTopLvl e1 ++ ", " ++ showTopLvl e2 ++ ")"
    OpVar opvar es _ -> show opvar ++ " (" ++ unwords (map showTopLvl es) ++ ")"

instance IsString (State Label Expr) where
  fromString = var_ . fromString

instance Labellable Expr where 
  toLabelValue a = textLabelValue $ pack $ showTopLvl a

--Abstract Interpreter specific-------------------------------------------------

instance HasLabel Expr where
  label e = case e of
    Lit _ l -> l
    List _ l -> l
    Cons _ _ l -> l
    Begin _ l -> l
    App _ _ l -> l
    Apply _ l -> l
  -- | Scheme expressions
    Var _ l -> l
    Set _ _ l -> l
    Define _ _ l -> l
    Lam _ _ l -> l
    If _ _ _ l -> l
    Let _ _ l -> l
    LetRec _ _ l -> l
  -- | Scheme standard procedures
    Op1 _ _ l -> l
    Op2 _ _ _ l -> l
    OpVar _ _ l -> l

instance Hashable Expr where
  hashWithSalt s e = s `hashWithSalt` label e

apply :: Prism' (store,(env,[Expr])) (store,(([Expr],Label),env))
apply = L.prism' (\(store,((es,l),env)) -> (store,(env, [Apply es l])))
                 (\(store,(env,e)) -> case e of
                      Apply es l:_ -> Just (store,((es,l),env))
                      _ -> Nothing)

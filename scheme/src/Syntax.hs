{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import           Data.Text (Text)
import           Data.Hashable
import           Data.Label
import           Data.String
import           Data.Lens (Prism')
import qualified Data.Lens as L
import           Data.GraphViz.Attributes
import           Data.Text.Prettyprint.Doc hiding (list)
import           Data.Text.Prettyprint.Doc.Render.Text

import           Control.Monad.State
import           Control.DeepSeq
import           GHC.Generics

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
  deriving (Eq,Generic,NFData)

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
  | ConsS -- (cons? z)
  | Car -- (car z)
  | Cdr -- (cdr z)
  | Caar -- (caar z)
  | Cadr -- (cadr z)
  | Cddr -- (cddr z)
  | Caddr -- (caddr z)
  | Error -- (error z)
  | Random -- (random z)
  deriving (Eq,Generic,NFData)

data Op2_
-- | Equivalence predicates
  = Eqv -- (eq? z) / (eqv? z)
-- | Numerical operations
  | Quotient -- (quotient z1 z2)
  | Remainder -- (remainder z1 z2)
  | Modulo -- (modulo z1 z2)
-- | List operations
  deriving (Eq,Generic,NFData)

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
-- | List operations
  deriving (Eq,Generic,NFData)


-- | Expressions of Scheme. Each expression has a label, with which the
-- expression can be uniquely identified.
data Expr
-- | Expressions for inner representation and evaluation
  = Lit Literal Label
  | Nil Label -- (nil)
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
  deriving (Generic,NFData)

instance Eq Expr where
  e1 == e2 = label e1 == label e2

-- Smart constructors that build labeled Scheme expressions.
-- | Expressions for inner representation and evaluation
lit :: Literal -> State Label Expr
lit x = Lit x <$> fresh
list :: [State Label Expr] -> State Label Expr
list [] = Nil <$> fresh
list (x : xs) = Cons <$> x <*> list xs <*> fresh
-- list_ :: [State Label Expr] -> State Label Expr 
-- list_ xs = List_ <$> (sequence xs) <*> fresh 
cons :: State Label Expr -> State Label Expr ->State Label Expr
cons e1 e2 = Cons <$> e1 <*> e2 <*> fresh
begin :: [State Label Expr] -> State Label Expr
begin es = Begin <$> sequence es <*> fresh
app :: State Label Expr -> [State Label Expr] -> State Label Expr
app e1 e2 = App <$> e1 <*> sequence e2 <*> fresh
-- | Scheme expressions
var_ :: Text -> State Label Expr
var_ x = Var x <$> fresh
set :: Text -> State Label Expr -> State Label Expr
set t e = Set t <$> e <*> fresh
lam :: [Text] -> [State Label Expr] -> State Label Expr
lam xs es = Lam xs <$> sequence es <*> fresh
if_ :: State Label Expr -> State Label Expr -> State Label Expr -> State Label Expr
if_ e1 e2 e3 = If <$> e1 <*> e2 <*> e3 <*> fresh
define :: Text -> State Label Expr -> State Label Expr
define t e = Define t <$> e <*> fresh
let_ :: [(Text, State Label Expr)] -> [State Label Expr] -> State Label Expr
let_ bnds body = Let <$> sequence [(v,) <$> e | (v,e) <- bnds] <*> sequence body <*> fresh
let_rec :: [(Text, State Label Expr)] -> [State Label Expr] -> State Label Expr
let_rec bnds body = LetRec <$> sequence [(v,) <$> e | (v,e) <- bnds] <*> sequence body <*> fresh
-- | Scheme standard procedures
op1_ :: Op1_ -> State Label Expr -> State Label Expr
op1_ operation e1 = Op1 operation <$> e1 <*> fresh
op2_ :: Op2_ -> State Label Expr -> State Label Expr -> State Label Expr
op2_ operation e1 e2 = Op2 operation <$> e1 <*> e2 <*> fresh
opvar_ :: OpVar_ -> [State Label Expr] -> State Label Expr
opvar_ operation es = OpVar operation <$> (sequence es) <*> fresh

instance Show Literal where show = show . pretty

instance Pretty Literal where
  pretty e0 = case e0 of
    Number x -> pretty x
    Float x -> pretty x
    Ratio x -> pretty (show x)
    Bool x -> pretty x
    Char x -> squotes (pretty x)
    String x -> dquotes (pretty x)
    Symbol x -> pretty x
    Quote x -> pretty x
    -- DottedList xs x -> showString ("DottedList ") . showList(xs) . showString (" . ") . shows (x)

instance Show Op1_ where show = show . pretty

instance Pretty Op1_ where
  pretty e0 = case e0 of
    Number_ -> "number?"
    Integer_ -> "integer?"
    Float_ -> "float?"
    Ratio_ -> "rational?"
    Zero -> "zero?"
    Positive -> "positive?"
    Negative -> "negative?"
    Odd -> "odd?"
    Even -> "even?"
    Abs -> "abs"
    Floor -> "floor"
    Ceiling -> "ceiling"
    Log -> "log"
    Boolean -> "boolean?"
    Not -> "not"
    Null -> "null?"
    ListS -> "list?"
    ConsS -> "cons?"
    Car -> "car"
    Cdr -> "cdr"
    Caar -> "caar"
    Cadr -> "cadr"
    Cddr -> "cddr"
    Caddr -> "caddr"
    Error -> "error"
    Random -> "random"

instance Show Op2_ where show = show . pretty

instance Pretty Op2_ where
  pretty e0 = case e0 of
    Eqv -> "eq?"
    Quotient -> "quotient "
    Remainder -> "remainder "
    Modulo -> "modulo "

instance Show OpVar_ where show = show . pretty

instance Pretty OpVar_ where
  pretty e0 = case e0 of
    EqualS -> "="
    SmallerS -> "<"
    GreaterS -> ">"
    SmallerEqualS -> "<="
    GreaterEqualS -> ">="
    Max -> "max"
    Min -> "min"
    Add -> "+"
    Mul -> "*"
    Sub -> "-"
    Div -> "/"
    Gcd -> "gcd"
    Lcm -> "lcm"
    -- List_ -> showString ("list")

instance Show Expr where show = show . pretty

instance Pretty Expr where
  pretty e0 = case e0 of
    Lit x _ -> pretty x
    Nil _ -> "nil"
    Cons e1 e2 _ -> parens $ "cons" <+> pretty e1 <+> pretty e2
    Begin es _-> parens $ "begin" <+> pretty es
    App e1 e2 _ -> parens $ pretty e1 <+> pretty e2
    Apply e _ -> parens $ pretty e
    Var x _ -> pretty x
    Set t e _ -> parens $ "set!" <+> pretty t <+> pretty e
    Define t e _ -> parens $ "define" <+> pretty t <+> pretty e
    Lam xs e2 _ -> parens $ "lambda" <+> prettyList xs <> "." <+> pretty e2
    If e1 e2 e3 _ -> parens $ "if" <+> pretty e1 <+> pretty e2 <+> pretty e3
    Let bnds body _ -> parens $ "let" <+> prettyList bnds <+> pretty body
    LetRec bnds body _ -> parens $ "letrec" <+> prettyList bnds <+> pretty body
    Op1 op1 e _ -> parens $ pretty op1 <+> pretty e
    Op2 op2 e1 e2 _ -> parens $ pretty op2 <+> pretty e1 <+> pretty e2
    OpVar opvar es _ -> parens $ pretty opvar <+> hsep (map pretty es)

showTopLvl :: Expr -> Doc ann
showTopLvl e = case e of
    Lit x _ -> "Lit" <+> pretty x
    Nil _ -> "Nil"
    Cons {} -> "Cons"
    Begin {} -> "Begin"
    App {} -> "App"
    Apply {} -> "Apply"
    Var x _ -> "Var" <+> pretty x
    Set t _ _ -> "Set" <+> pretty t <+> showTopLvl e
    Define t _ _ -> "Define" <+> pretty t <+> showTopLvl e
    Lam xs e2 _ -> "Lam" <+> hsep (map pretty xs) <+> "->" <+> hsep (map showTopLvl e2)
    If e1 _ _ _ -> "If" <+> parens (showTopLvl e1)
    Let {} -> "Let"
    LetRec {} -> "LetRec"
    Op1 op1 _ _ -> pretty op1 <+> parens (showTopLvl e)
    Op2 op2 e1 e2 _ -> pretty op2 <+> parens (showTopLvl e1 <> "," <> showTopLvl e2)
    OpVar opvar es _ -> pretty opvar <+> parens (hsep (map showTopLvl es))

instance IsString (State Label Expr) where
  fromString = var_ . fromString

instance Labellable Expr where
  toLabelValue a = textLabelValue $ renderLazy $ layoutPretty defaultLayoutOptions $ showTopLvl a

instance HasLabel Expr where
  label e = case e of
    Lit _ l -> l
    Nil l -> l
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

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
import           Data.GraphViz.Attributes
import           Data.Text.Prettyprint.Doc hiding (list)
import           Data.Text.Prettyprint.Doc.Render.Text

import           Control.Monad.State
import           Control.DeepSeq
import           GHC.Generics

-- Literals of Scheme
data Literal
  = Int Int
  | Float Double
  | Rational Rational
  | Bool Bool
  | Char Char --single character and single quotation (')
  | String Text -- any amount of chars and double quotation (")
  | Symbol Text
  | Quote Literal
  -- | DottedList [Literal] Literal
  deriving (Eq,Generic,NFData)

data Op1
-- | Check operations
  = IsNumber -- (number? z)
  | IsInteger -- (integer? z)
  | IsFloat -- (float? z)
  | IsRational -- (rational? z)
  | IsZero -- (zero? z)
  | IsPositive -- (positive? z)
  | IsNegative -- (negative? z)
  | IsOdd -- (odd? z)
  | IsEven-- (even? z)
  | IsNull -- (null? z)
  | IsCons -- (cons? z)
  | IsBoolean -- (boolean? z)
-- | Numeric Operations
  | Abs -- (abs z)
  | Floor -- (floor z)
  | Ceiling -- (ceiling z)
  | Log -- (log z) base e
-- | Boolean operations
  | Not -- (not z)
-- | List operations
  | Car -- (car z)
  | Cdr -- (cdr z)
  | Caar -- (caar z)
  | Cadr -- (cadr z)
  | Cddr -- (cddr z)
  | Caddr -- (caddr z)
  | Cadddr -- (cadddr z)
-- | String Operations
  | NumberToString -- (number->string z)
  | StringToSymbol -- (string->symbol z)
  | SymbolToString -- (symbol->string z)
-- | Miscellaneous operations
  | Error -- (error z)
  | Random -- (random z)
  deriving (Eq,Generic,NFData)

data Op2
-- | Equivalence predicates
  = Eqv -- (eq? z) / (eqv? z)
-- | Numerical operations
  | Quotient -- (quotient z1 z2)
  | Remainder -- (remainder z1 z2)
  | Modulo -- (modulo z1 z2)
-- | String operations
  | StringRef -- (string-ref z1 z2)
  deriving (Eq,Generic,NFData)

data OpVar
-- | Numerical operations
  = Equal -- (= z1 z2 z3 ...)
  | Smaller -- (< z1 z2 z3 ...)
  | Greater -- (> z1 z2 z3 ...)
  | SmallerEqual -- (<= z1 z2 z3 ...)
  | GreaterEqual -- (>= z1 z2 z3 ...)
  | Max -- (max z1 z2 z3 ...)
  | Min -- (max z1 z2 z3 ...)
  | Add -- (+) (+ z) (+ z1 z2 z3 ...)
  | Mul -- (*) (* z) (* z1 z2 z3 ...)
  | Sub -- (- z) (- z1 z2 z3 ...)
  | Div -- (/ z) (/ z1 z2 z3 ...)
  | Gcd -- (gcd z1 z2 z3 ...)
  | Lcm -- (lcm z1 z2 z3 ...)
  | StringAppend -- (string-append z1 z2 z3 ...)
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
  | Op1 Op1 Expr Label
  | Op2 Op2 Expr Expr Label
  | OpVar OpVar [Expr] Label
  deriving (Generic,NFData)

instance Eq Expr where
  e1 == e2 = label e1 == label e2

type LExpr = State Label Expr

-- Smart constructors that build labeled Scheme expressions.
-- | Expressions for inner representation and evaluation
lit :: Literal -> LExpr
lit x = Lit x <$> fresh
list :: [LExpr] -> LExpr
list [] = Nil <$> fresh
list (x : xs) = Cons <$> x <*> list xs <*> fresh
-- list_ :: [State Label Expr] -> State Label Expr 
-- list_ xs = List_ <$> (sequence xs) <*> fresh 
cons :: LExpr -> LExpr ->LExpr
cons e1 e2 = Cons <$> e1 <*> e2 <*> fresh
begin :: [LExpr] -> LExpr
begin es = Begin <$> sequence es <*> fresh
app :: LExpr -> [LExpr] -> LExpr
app e1 e2 = App <$> e1 <*> sequence e2 <*> fresh
-- | Scheme expressions
var_ :: Text -> LExpr
var_ x = Var x <$> fresh
set :: Text -> LExpr -> LExpr
set t e = Set t <$> e <*> fresh
lam :: [Text] -> [LExpr] -> LExpr
lam xs es = Lam xs <$> sequence es <*> fresh
if_ :: LExpr -> LExpr -> LExpr -> LExpr
if_ e1 e2 e3 = If <$> e1 <*> e2 <*> e3 <*> fresh
define :: Text -> LExpr -> LExpr
define t e = Define t <$> e <*> fresh
let_ :: [(Text, LExpr)] -> [LExpr] -> LExpr
let_ bnds body = Let <$> sequence [(v,) <$> e | (v,e) <- bnds] <*> sequence body <*> fresh
let_rec :: [(Text, LExpr)] -> [LExpr] -> LExpr
let_rec bnds body = LetRec <$> sequence [(v,) <$> e | (v,e) <- bnds] <*> sequence body <*> fresh
-- | Scheme standard procedures
op1_ :: Op1 -> LExpr -> LExpr
op1_ operation e1 = Op1 operation <$> e1 <*> fresh
op2_ :: Op2 -> LExpr -> LExpr -> LExpr
op2_ operation e1 e2 = Op2 operation <$> e1 <*> e2 <*> fresh
opvar_ :: OpVar -> [LExpr] -> LExpr
opvar_ operation es = OpVar operation <$> sequence es <*> fresh

instance Show Literal where show = show . pretty

instance Pretty Literal where
  pretty e0 = case e0 of
    Int x -> pretty x
    Float x -> pretty x
    Rational x -> pretty (show x)
    Bool x -> pretty x
    Char x -> squotes (pretty x)
    String x -> dquotes (pretty x)
    Symbol x -> pretty x
    Quote x -> "'" <> pretty x
    -- DottedList xs x -> showString ("DottedList ") . showList(xs) . showString (" . ") . shows (x)

instance Show Op1 where show = show . pretty

instance Pretty Op1 where
  pretty e0 = case e0 of
    IsNumber -> "number?"
    IsInteger -> "integer?"
    IsFloat -> "float?"
    IsRational -> "rational?"
    IsZero -> "zero?"
    IsPositive -> "positive?"
    IsNegative -> "negative?"
    IsOdd -> "odd?"
    IsEven -> "even?"
    IsBoolean -> "boolean?"
    IsNull -> "null?"
    IsCons -> "cons?"
    Abs -> "abs"
    Floor -> "floor"
    Ceiling -> "ceiling"
    Log -> "log"
    Not -> "not"
    Car -> "car"
    Cdr -> "cdr"
    Caar -> "caar"
    Cadr -> "cadr"
    Cddr -> "cddr"
    Caddr -> "caddr"
    Cadddr -> "cadddr"
    Error -> "error"
    Random -> "random"
    StringToSymbol -> "string->symbol"
    SymbolToString -> "symbol->string"
    NumberToString -> "number->string"

instance Show Op2 where show = show . pretty

instance Pretty Op2 where
  pretty e0 = case e0 of
    Eqv -> "eq?"
    Quotient -> "quotient "
    Remainder -> "remainder "
    Modulo -> "modulo "
    StringRef -> "string-ref "

instance Show OpVar where show = show . pretty

instance Pretty OpVar where
  pretty e0 = case e0 of
    Equal -> "="
    Smaller -> "<"
    Greater -> ">"
    SmallerEqual -> "<="
    GreaterEqual -> ">="
    Max -> "max"
    Min -> "min"
    Add -> "+"
    Mul -> "*"
    Sub -> "-"
    Div -> "/"
    Gcd -> "gcd"
    Lcm -> "lcm"
    StringAppend -> "string-append"
    -- List_ -> showString ("list")

instance Show Expr where show = show . pretty

instance Pretty Expr where
  pretty e = flatAlt (prettyExpr e) (parens (showTopLvl e <> "..."))

prettyExpr :: Expr -> Doc ann
prettyExpr e0 = case e0 of
  Lit x _ -> pretty x
  Nil _ -> "nil"
  Cons e1 e2 _ -> parens $ "cons" <+> prettyExpr e1 <+> prettyExpr e2
  Begin es _-> parens $ "begin" <+> prettyExprList es
  App e1 e2 _ -> parens $ prettyExpr e1 <+> prettyExprList e2
  Apply e _ -> parens $ prettyExprList e
  Var x _ -> pretty x
  Set t e _ -> parens $ "set!" <+> pretty t <+> prettyExpr e
  Define t e _ -> parens $ "define" <+> pretty t <+> prettyExpr e
  Lam xs e2 _ -> parens $ "lambda" <+> hsep (map pretty xs) <> "." <+> prettyExprList e2
  If e1 e2 e3 _ -> parens $ "if" <+> prettyExpr e1 <+> prettyExpr e2 <+> prettyExpr e3
  Let bnds body _ -> parens $ "let" <+> brackets (align (vcat [ pretty var <+> prettyExpr expr | (var,expr) <- bnds])) <+> prettyExprList body
  LetRec bnds body _ -> parens $ "letrec" <+> brackets (align (vcat [ pretty var <+> prettyExpr expr | (var,expr) <- bnds])) <+> pretty body
  Op1 op1 e _ -> parens $ pretty op1 <+> prettyExpr e
  Op2 op2 e1 e2 _ -> parens $ pretty op2 <> prettyExpr e1 <+> prettyExpr e2
  OpVar opvar es _ -> parens $ pretty opvar <+> prettyExprList es

prettyExprList :: [Expr] -> Doc ann
prettyExprList expr = hsep (map prettyExpr expr)

showTopLvl :: Expr -> Doc ann
showTopLvl e = case e of
    Lit x _ -> pretty x
    Nil _ -> "nil"
    Cons {} -> "cons"
    Begin {} -> "begin"
    App {} -> "app"
    Apply {} -> "apply"
    Var x _ -> pretty x
    Set t _ _ -> "set!" <+> pretty t
    Define t _ _ -> "define" <+> pretty t
    Lam xs _ _ -> "λ" <+> hsep (map pretty xs)
    If e1 _ _ _ -> "if" <+> pretty e1
    Let {} -> "let"
    LetRec {} -> "letrec"
    Op1 op1 e1 _ -> pretty op1 <+> showTopLvl e1
    Op2 op2 e1 e2 _ -> pretty op2 <+> showTopLvl e1 <> "," <> showTopLvl e2
    OpVar opvar es _ -> pretty opvar <+> hsep (map showTopLvl es)

controlFlow :: Expr -> Maybe Expr
controlFlow e = case e of
  App {} -> Just e
  LetRec {} -> Just e
  _ -> Nothing

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


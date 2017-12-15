{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Grammar.RegularTreeGrammar where

import           WildcardSemantics (Term)
import qualified WildcardSemantics as W

import           Data.Constructor
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Hashable (Hashable(..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text,unpack)
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer

data Symbol = Symbol {-# UNPACK #-} !Int {-# UNPACK #-} !Text 

data GrammarTerm
  = Cons Constructor [GrammarTerm]
  | AnyString
  | StringLiteral Text
  | AnyNumber
  | NumberLiteral Int
  | NonTerminal Symbol
  deriving (Eq)

data Rule = Rule {-# UNPACK #-} !Symbol GrammarTerm deriving Eq

type Grammar = Vector Rule

type Summary = HashMap Rule Int

summary :: Grammar -> Term -> Maybe Summary
summary g = fmap (foldl' (flip (M.adjust (+ 1))) initial) . runTopDown g
  where
    initial :: Summary
    initial = M.fromList $ (,0) <$> V.toList g

runTopDown :: Grammar -> Term -> Maybe (Seq Rule)
runTopDown grammar term = execWriterT (runReaderT (topDown (Symbol 0 "") term) grammar)

distanceSum :: Grammar -> Term -> Maybe Int
distanceSum g t = S.length <$> runTopDown g t

topDown :: (MonadReader Grammar m, MonadWriter (Seq Rule) m, MonadPlus m) => Symbol -> Term -> m ()
topDown startSymbol term = do
  grammar <- ask
  go grammar
  where
    go :: (MonadReader Grammar m, MonadWriter (Seq Rule) m, MonadPlus m) => Grammar -> m ()
    go = V.foldr (\rule@(Rule symbol rhs) -> mplus (guard (startSymbol == symbol) >> match rhs term >> tell (return rule))) mzero

    match :: (MonadReader Grammar m, MonadWriter (Seq Rule) m, MonadPlus m) => GrammarTerm -> Term -> m ()
    match (Cons s ts) (W.Cons s' ts') | s == s' && length ts == length ts' = zipWithM_ match ts ts'

    match AnyString (W.StringLiteral _) = return ()
    match (StringLiteral s) (W.StringLiteral s') = guard $ s == s'

    match AnyNumber (W.NumberLiteral _) = return ()
    match (NumberLiteral n) (W.NumberLiteral n') = guard $ n == n'

    match _ W.Wildcard = return ()

    match (NonTerminal n) t = topDown n t

    match _ _ = mzero

termsOfDistance :: Int -> Grammar -> HashSet Term
termsOfDistance maxDepth grammar = H.fromList $ toList $ go maxDepth (Symbol 0 "")
  where
    go :: Int -> Symbol -> Seq Term
    go depth startSym = foldMap (\(Rule sym term) -> guard (startSym == sym) >> goTerm depth term) grammar

    goTerm :: Int -> GrammarTerm -> Seq Term
    goTerm 0 _ = mempty
    goTerm depth term = case term of
      Cons c ts -> W.Cons c <$> mapM (goTerm depth) ts
      AnyString -> return W.Wildcard
      StringLiteral t -> return $ W.StringLiteral t
      AnyNumber -> return W.Wildcard
      NumberLiteral t -> return $ W.NumberLiteral t
      NonTerminal t -> go (depth - 1) t

{-
Start Symbol: NF

NF -> Zero()
NF -> Succ(NF)
NF -> Pred(NF)
NF -> Abs(String,Type,Expr)

Expr -> Zero()
Expr -> Succ(Expr)
Expr -> Pred(Expr)
Expr -> Var(String)
Expr -> App(Expr,Expr)
Expr -> Abs(String,Type,Expr)
Expr -> Ifz(Expr,Expr,Expr)

Type -> Num()
Type -> Fun(Type,Type)
-}
pcfEvalGrammar :: Grammar
pcfEvalGrammar =
  V.fromList [
    Rule nf $ Cons "Zero" [],
    Rule nf $ Cons "Succ" [nf'],
    Rule nf $ Cons "Pred" [nf'],
    Rule nf $ Cons "Abs" [AnyString, typ', expr'],

    Rule expr $ Cons "Var" [AnyString],
    Rule expr $ Cons "App" [expr',expr'],
    Rule expr $ Cons "Abs" [AnyString, typ', expr'],
    Rule expr $ Cons "Zero" [],
    Rule expr $ Cons "Succ" [expr'],
    Rule expr $ Cons "Pred" [expr'],
    Rule expr $ Cons "Ifz" [expr', expr', expr'],

    Rule typ $ Cons "Num" [],
    Rule typ $ Cons "Fun" [typ', typ']
  ]
  where
    nf = Symbol 0 "NF"
    nf' = NonTerminal nf
    expr = Symbol 1 "Expr"
    expr' = NonTerminal expr
    typ = Symbol 2 "Type"
    typ' = NonTerminal typ

{-
Start Symbol: Value

Value -> Num
Value -> Abs(String,Type,Expr)

Num -> Zero()
Num -> Pred(Num)
Num -> Succ(Num)

Expr -> Var(String)
Expr -> App(Expr,Expr)
Expr -> Abs(String,Type,Expr)
Expr -> Zero
Expr -> Succ(Expr)
Expr -> Pred(Expr)
Expr -> Ifz(Expr,Expr,Expr)

Type -> Num()
Type -> Fun(Type,Type)
-}
pcfCheckEvalGrammar :: Grammar
pcfCheckEvalGrammar =
  V.fromList [
    Rule value num',
    Rule value $ Cons "Abs" [AnyString, typ', expr'],

    Rule num $ Cons "Zero" [],
    Rule num $ Cons "Succ" [num'],
    Rule num $ Cons "Pred" [num'],

    Rule expr $ Cons "Var" [AnyString],
    Rule expr $ Cons "App" [expr',expr'],
    Rule expr $ Cons "Abs" [AnyString, typ', expr'],
    Rule expr $ Cons "Zero" [],
    Rule expr $ Cons "Succ" [expr'],
    Rule expr $ Cons "Pred" [expr'],
    Rule expr $ Cons "Ifz" [expr', expr', expr'],

    Rule typ $ Cons "Num" [],
    Rule typ $ Cons "Fun" [typ', typ']
  ]
  where
    value = Symbol 0 "Value"
    num = Symbol 1 "Num"
    num' = NonTerminal num
    expr = Symbol 2 "Expr"
    expr' = NonTerminal expr
    typ = Symbol 3 "Type"
    typ' = NonTerminal typ

instance Eq Symbol where
  Symbol i _ == Symbol j _ = i == j

instance Ord Symbol where
  Symbol i _ <= Symbol j _ = i <= j
  Symbol i _ `compare` Symbol j _ = i `compare` j

instance Show Symbol where
  show (Symbol _ t) = unpack t

instance Hashable Symbol where
  hashWithSalt s (Symbol i _) = s `hashWithSalt` i

instance Show Rule where
  show (Rule from to) = show from ++ " -> " ++ show to

instance Show GrammarTerm where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show AnyString = "String"
  show (StringLiteral s) = show s
  show AnyNumber = "Int"
  show (NumberLiteral n) = show n
  show (NonTerminal n) = show n

instance Hashable GrammarTerm where
  hashWithSalt s (Cons c ts) = s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts
  hashWithSalt s AnyString = s `hashWithSalt` (1::Int)
  hashWithSalt s (StringLiteral t) = s `hashWithSalt` (2::Int) `hashWithSalt` t
  hashWithSalt s AnyNumber = s `hashWithSalt` (3::Int)
  hashWithSalt s (NumberLiteral n) = s `hashWithSalt` (4::Int) `hashWithSalt` n
  hashWithSalt s (NonTerminal n) = s `hashWithSalt` (5::Int) `hashWithSalt` n

instance Hashable Rule where
  hashWithSalt s (Rule sym term) = s `hashWithSalt` sym `hashWithSalt` term

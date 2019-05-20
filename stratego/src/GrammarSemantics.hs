{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-warn-orphans #-}
module GrammarSemantics where

import           Prelude hiding (fail)

import qualified ConcreteSemantics as C
import           SharedSemantics as Shared
import           AbstractSemantics
import           SortContext(Context,Signature(..),Sort,sorts)
import qualified SortContext as Ctx
import           Syntax hiding (Fail)
import           Utils

import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Const
import           Control.Arrow.Abstract.Join
import qualified Control.Monad as M

import           Data.Coerce
import           Data.Identifiable
import qualified Data.Concrete.Powerset as C
import           Data.Constructor
import           Data.GaloisConnection
import qualified Data.HashMap.Lazy as LM
import           Data.Hashable
import           Data.Order
import           Data.Term
import           Data.TermEnvironment
import           Data.Text (Text)
import           Data.Profunctor
import qualified Data.Abstract.Either as A
import           Data.Abstract.FreeCompletion (FreeCompletion)
import qualified Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Except (Except)
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (Terminating,fromTerminating)
import           Data.Abstract.Widening as W
import           Data.Abstract.DiscretePowerset(Pow)
import           Data.Abstract.TreeGrammar
import qualified Data.Abstract.TreeGrammar.Terminal as Term

import qualified Test.QuickCheck as Q
import           Text.Printf
import           GHC.Exts


data Constr n = Bot | Constr (Term.Constr n) | StringLit (FreeCompletion Text) | NumLit (FreeCompletion Int) | Top deriving (Eq)
newtype Term = Term (Grammar Int Constr) deriving (Complete, Eq, Hashable, PreOrd)

type Stack = SW.Groups StratVar SW.Stack

eval :: Int -> Int -> Strat -> StratEnv -> Context -> TermEnv Term -> Term -> Terminating (FreeCompletion (Error TypeError (Except () (TermEnv Term,Term))))
eval i _ strat senv ctx  = runInterp (Shared.eval' strat) i widening senv ctx


-- Instances -----------------------------------------------------------------------------------------
instance Complete (FreeCompletion Term) where
  Free.Lower x ⊔ Free.Lower y = Free.Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance Complete (FreeCompletion (Grammar Int Constr)) where
  Free.Lower x ⊔ Free.Lower y = Free.Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance (IsString e, ArrowFail e c, ArrowJoin c, ArrowExcept () c, ArrowChoice c, Profunctor c, LowerBounded (c () Term))
   => IsTerm Term (ValueT Term c) where
  matchCons matchSubterms = proc (Constructor c,ps,Term g) ->
    case toSubterms g of
      Bot -> bottom -< ()
      Constr cs ->
        (| joinList (bottom -< ()) (\(c',ts) ->
            if c' == c && eqLength ps ts
            then do
              ts' <- matchSubterms -< (ps,ts)
              buildCons -< (Constructor c,ts')
            else throw -< ()) |) (coerce (toList cs))
      NumLit _ -> throw -< ()
      StringLit _ -> throw -< ()
      Top -> (do ts' <- matchSubterms -< (ps,[ topGrammar | _ <- ps ]); buildCons -< (Constructor c,ts'))
         <⊔> (throw -< ())

  matchExplode _ _ = error "unsupported"

  matchNum = proc (n,Term g) -> case toSubterms g of
    Bot -> bottom -< ()
    NumLit (Free.Lower n')
      | n == n' -> returnA -< Term g
      | otherwise -> throw -< ()
    Constr _ -> throw -< ()
    StringLit _ -> throw -< ()
    _ -> (returnA -< Term g) <⊔> (throw -< ())

  matchString = proc (s,Term g) -> case toSubterms g of
    Bot -> bottom -< ()
    StringLit (Free.Lower s')
      | s == s' -> returnA -< Term g
      | otherwise -> throw -< ()
    Constr _ -> throw -< ()
    NumLit _ -> throw -< ()
    _ -> (returnA -< Term g) <⊔> (throw -< ())

  buildCons = arr $ \(Constructor c,ts) -> constr c ts
  buildNum = arr numLit
  buildString = arr stringLit
  buildExplode = error "unsupported"

  equal = proc (Term g1, Term g2) -> case g1 ⊓ g2 of
    g | isEmpty g -> throw -< ()
      -- isSingleton g1 && isSingleton g2 -> returnA -< Term g
      | otherwise -> (returnA -< Term g) <⊔> (throw -< ())

  mapSubterms f = proc (Term g) ->
    case toSubterms g of
      Bot -> bottom -< ()
      Constr cs -> 
        (| joinList (bottom -< ()) (\(c,ts) -> do
           ts' <- f -< ts
           buildCons -< (Constructor c,ts')) |)
            (coerce (toList cs))
      StringLit _ -> returnA -< Term g
      NumLit _ -> returnA -< Term g
      Top -> fail -< "cannot map over the subterms of Top"

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> constr "Cons" [x, convertToList xs]
    [] -> constr "Nil" []
  size (Term _) = error "not implemented: TreeAutomata.size g"
  height (Term _) = error "not implemented: TreeAutomata.height g"

instance Galois (C.Pow C.Term) Term where
  alpha = lub . fmap go
    where
      go (C.Cons (Constructor c) ts) = constr c (fmap go ts)
      go (C.StringLiteral s) = stringLit s
      go (C.NumberLiteral n) = numLit n
  gamma = error "Uncomputable"

sound :: (Identifiable b1, Show b2, Complete b2, Galois (C.Pow a1) a2, Galois (C.Pow b1) b2)
      => Int -> StratEnv -> C.Pow (a1, C.TermEnv) -> C.Interp a1 b1 -> Interp Term _ a2 b2 -> Q.Property
sound i senv xs f g =
  let con :: FreeCompletion (Error (Pow String) (Except () (TermEnv Term,_)))
      con = Free.Lower (alpha (fmap (\(x,tenv) -> C.runInterp f senv tenv x) xs))
      abst :: FreeCompletion (Error (Pow String) (Except () (TermEnv Term,_)))
      -- TODO: using fromTerminating is a bit of a hack...
      abst = fromTerminating Free.Top $ runInterp g i widening senv Ctx.empty (alpha (fmap snd xs)) (alpha (fmap fst xs))
  in Q.counterexample (printf "%s ⊑/ %s" (show con) (show abst)) $ con ⊑ abst

-- Helpers -------------------------------------------------------------------------------------------
widening :: Widening Term
widening = undefined
-- widening (Term t1) (Term t2) = Term (widen t1 t2)

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd _ [] = []
mapSnd f ((x,y):s) = (x,f y) : mapSnd f s

constr :: Text -> [Term] -> Term
constr c ts = Term $ fromSubterms $ Constr $ fromList [(c,coerce ts::[Grammar Int Constr])]

stringLit :: Text -> Term
stringLit s = Term (grammar "S" [("S",StringLit (return s))] [])

numLit :: Int -> Term
numLit n = Term (grammar "S" [("S",NumLit (return n))] [])

topGrammar :: Term
topGrammar = Term (grammar "S" [("S",Top)] [])

instance ArrowConst Context c => ArrowTop Term (EnvironmentT Term c) where
  topA = proc () -> returnA -< topGrammar

instance Term.Terminal Constr where
  nonTerminals (Constr cs) = Term.nonTerminals cs
  nonTerminals _ = mempty
  
  productive _ Bot = False
  productive f (Constr cs) = Term.productive f cs
  productive _ _ = True

  filter f (Constr m) = Constr (Term.filter f m)
  filter _ t = t

  determinize f (Constr m) = Constr <$> (Term.determinize f m)
  determinize _ Top = pure Top
  determinize _ (StringLit s) = pure (StringLit s)
  determinize _ (NumLit s) = pure (NumLit s)
  determinize _ Bot = pure Bot

  subsetOf _ _ Top = return ()
  subsetOf _ Bot _ = return ()
  subsetOf _ (StringLit s) (StringLit s') = M.guard (s ⊑ s')
  subsetOf _ (NumLit n) (NumLit n') = M.guard (n ⊑ n')
  subsetOf f (Constr ns) (Constr ns') = Term.subsetOf f ns ns'
  subsetOf _ _ _ = M.mzero
                      
  union f Bot a = Term.traverse (f . A.Right) a
  union f a Bot = Term.traverse (f . A.Left) a
  union _ (StringLit s) (StringLit s') = pure (StringLit (s ⊔ s'))
  union _ (NumLit n) (NumLit n') = pure (NumLit (n ⊔ n'))
  union f (Constr cs) (Constr cs') = Constr <$> Term.union f cs cs'
  union _ _ _ = pure Top

  intersection _ (StringLit s) (StringLit s')
    | s == s' = pure (StringLit s)
    | otherwise = pure Bot
  intersection _ (NumLit n) (NumLit n')
    | n == n' = pure (NumLit n)
    | otherwise = pure Bot
  intersection f (Constr cs) (Constr cs') = Constr <$> Term.intersection f cs cs'
  intersection _ Top Top = pure Top
  intersection _ _ _ = pure Bot
  
  traverse _ Top = pure Top
  traverse _ Bot = pure Bot
  traverse _ (StringLit s) = pure (StringLit s)
  traverse _ (NumLit n) = pure (NumLit n)
  traverse f (Constr cs) = Constr <$> Term.traverse f cs

  hashWithSalt _ s Bot           = pure (s `hashWithSalt` (1 :: Int))
  hashWithSalt _ s (StringLit x) = pure (s `hashWithSalt` (2 :: Int) `hashWithSalt` x)
  hashWithSalt _ s (NumLit x)    = pure (s `hashWithSalt` (3 :: Int) `hashWithSalt` x)
  hashWithSalt _ s Top           = pure (s `hashWithSalt` (4 :: Int))
  hashWithSalt f s (Constr cs)   = Term.hashWithSalt f (s `hashWithSalt` (5 :: Int)) cs

instance Identifiable n => Monoid (Constr n) where
  mempty = Bot
  mappend = (<>)

instance Identifiable n => Semigroup (Constr n) where
  Bot <> y = y
  x <> Bot = x
  StringLit s <> StringLit s' = StringLit (s ⊔ s')
  NumLit n <> NumLit n' = NumLit (n ⊔ n')
  Constr ts <> Constr ts' = Constr (ts <> ts')
  _ <> _ = Top

instance (Identifiable n, Show n) => Show (Constr n) where
  show Top = "⊤"
  show Bot = "⊥"
  show (StringLit s) = "String " ++ show s
  show (NumLit n) = "Number " ++ show n
  show (Constr cs) = show cs

instance Show Term where
  show (Term t) = show (dropUnreachable t)

fromContext :: Context -> Term
fromContext ctx = Term $ grammar "Start" (map toProd (LM.toList (sorts ctx))) [("Start", map show $ LM.keys (sorts ctx))]
  where
    toProd :: (Sort,[(Constructor,Signature)]) -> (String, Constr String)
    toProd (sort, rhss) = (show sort,Constr (fromList (map (toText *** (\(Signature args _) -> map show args)) rhss)))
    

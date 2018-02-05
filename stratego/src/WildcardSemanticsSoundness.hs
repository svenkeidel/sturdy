{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemanticsSoundness where

import           Prelude hiding (Ord(..),abs)

import qualified ConcreteSemantics as C
import           Syntax (Strat,StratEnv)
import qualified WildcardSemantics as W
import qualified WildcardSemanticsDelayed as W

import           Control.Arrow

import           Data.Foldable (toList)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S

import           Text.Printf

import           Test.QuickCheck

alphaTerm :: C.Term -> W.Term
alphaTerm t = case t of
  C.Cons c ts -> W.Cons c (alphaTerm <$> ts)
  C.StringLiteral s -> W.StringLiteral s
  C.NumberLiteral n -> W.NumberLiteral n

alphaEnv :: C.TermEnv -> W.TermEnv
alphaEnv = fmap alphaTerm

alphaResult :: Seq (Either () (C.Term,C.TermEnv)) -> Seq (Either () (W.Term,W.TermEnv))
alphaResult = (fmap.fmap) (alphaTerm *** alphaEnv)

sound'' :: Int -> Strat -> Property
sound'' i s = property $ do
  (t1,t2) <- C.similar
  return $ sound' i s (S.fromList [t1,t2])

sound' :: Int -> Strat -> Seq C.Term -> Property
sound' i s ts = sound i M.empty s (fmap (id &&& const M.empty) ts)

{-         P (C.eval s)
P (T x E) --------------> P (R (T x E))
   |^                          |^
   ||                          ||
   v|        W.eval s       >= v|
 T' x E' --------------> P (R (T' x E'))
-}
sound :: Int -> StratEnv -> Strat -> Seq (C.Term,C.TermEnv) -> Property
sound i senv s ts =
  let abs = W.eval i senv s $ alphaDom ts
      con = alphaResult $ C.eval senv s <$> ts
  in counterexample (printf "%s < %s" (show (toList abs)) (show (toList con)))
       (con <= abs)

alphaDom :: Seq (C.Term,C.TermEnv) -> (W.Term,W.TermEnv)
alphaDom = lubs . fmap (alphaTerm *** alphaEnv)

isWittness :: W.Term -> HashSet W.Term -> Bool
isWittness t = any (\t' -> t <= t' && t /= t')

class PartOrd a where
  (<=) :: a -> a -> Bool

class PartOrd a => Lattice a where
  lub :: a -> a -> a

lubs :: (Lattice a,Foldable f) => f a -> a
lubs = foldl1 lub

instance PartOrd W.TermEnv where
  e1 <= e2 = M.foldr (&&) True (M.intersectionWith (<=) e1 e2)

instance Lattice W.TermEnv where
  lub = M.intersectionWith lub

instance PartOrd W.Term where
  t1 <= t2 = case (t1,t2) of
    (W.Cons c1 ts1, W.Cons c2 ts2) -> c1 == c2 && all (uncurry (<=)) (zip ts1 ts2)
    (W.StringLiteral s1, W.StringLiteral s2) -> s1 == s2
    (W.NumberLiteral n1, W.NumberLiteral n2) -> n1 == n2
    (_,W.Wildcard) -> True
    _ -> False

instance Lattice W.Term where
  lub t1 t2 = case (t1,t2) of
    (W.Cons c ts, W.Cons c' ts')
      | c == c' && length ts == length ts' -> W.Cons c (zipWith lub ts ts')
      | otherwise -> W.Wildcard
    (W.StringLiteral s, W.StringLiteral s')
      | s == s' -> W.StringLiteral s
      | otherwise -> W.Wildcard
    (W.NumberLiteral n, W.NumberLiteral n')
      | n == n' -> W.NumberLiteral n
      | otherwise -> W.Wildcard
    (_, _) -> W.Wildcard

instance (PartOrd a,PartOrd b) => PartOrd (a,b) where
  (a1,b1) <= (a2,b2) = a1 <= a2 && b1 <= b2

instance (Lattice a, Lattice b) => Lattice (a,b) where
  lub (a1,b1) (a2,b2) = (lub a1 a2, lub b1 b2)

instance PartOrd a => PartOrd (Either () a) where
  r1 <= r2 = case (r1,r2) of
    (Fail,Fail) -> True
    (Success a1,Success a2) -> a1 <= a2
    _ -> False

instance PartOrd a => PartOrd (Seq a) where
  s1 <= s2 = and $ do
    a <- s1
    return $ or $ do
      b <- s2
      return $ a <= b

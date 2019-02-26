-- Copyright (c) 2018, Michael D. Adams
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Michael D. Adams nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.TreeGrammar
( Grammar
, IsGrammar
, union
, epsilonClosure
, dropUnreachable
, dropUnproductive
, productive
, determinize
, subsetOf
, subsetOf'
, toSubterms
, fromSubterms

, GrammarBuilder
, fromGrammar
, getGrammar
, grammar
)
where

import           Prelude hiding (lookup)

import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Maybe
import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.Foldable
import qualified Data.List as L
import           Data.Order

import           Data.Abstract.TreeGrammar.NonTerminal
import           Data.Abstract.TreeGrammar.Terminal (Terminal)
import qualified Data.Abstract.TreeGrammar.Terminal as T

import           Text.Printf


type Identifiable a = (Hashable a, Eq a)

data Grammar n t = Grammar { start :: n, prods :: ProdMap n t }
type ProdMap n t = HashMap n (Rhs n t)
data Rhs n t = Rhs { cons :: t n, eps :: HashSet n }
type IsGrammar n t = (Terminal t, Monoid (t n), NonTerminal n, Identifiable n)

data GrammarBuilder n t = GrammarBuilder (State (Gen n) (Grammar n t)) (Grammar n t)

grammarBuilder :: NonTerminal n => State (Gen n) (Grammar n t) -> GrammarBuilder n t
grammarBuilder f = GrammarBuilder f (generate f)

runGrammarBuilder :: GrammarBuilder n t -> State (Gen n) (Grammar n t)
runGrammarBuilder (GrammarBuilder f _) = f

getGrammar :: GrammarBuilder n t -> Grammar n t
getGrammar (GrammarBuilder _ g) = g

fromGrammar :: (Identifiable n, IsGrammar n' t) => Grammar n t -> GrammarBuilder n' t
fromGrammar g = grammarBuilder $ do
  i <- get
  let ((s'',p'),(i',_)) = runState (do
        s' <- go (start g)
        prods' <- traverse (\(n,r) -> do
                      n' <- go n
                      c' <- T.traverse go (cons r)
                      e' <- traverse go (Set.toList (eps r))
                      return (n',Rhs c' (Set.fromList e')))
                    (Map.toList (prods g))
        return (s',Map.fromList prods'))
        (i,mempty)
  put i'
  return $ Grammar s'' p'
  where
    go :: (Identifiable n, NonTerminal n') => n -> State (Gen n', HashMap n n') n'
    go n = do
      (_,rmap) <- get
      case Map.lookup n rmap of
        Just x -> return x
        Nothing -> do
          n' <- fresh' Nothing
          modify $ second $ Map.insert n n'
          return n'

grammar :: (IsGrammar n t, Monoid (t String))
        => String -> [(String,t String)] -> [(String,[String])] -> GrammarBuilder n t
grammar s prods eps = fromGrammar
                    $ Grammar s
                    $ Map.fromListWith (<>)
                    $ [ (n,Rhs c mempty) | (n,c) <- prods ]
                   ++ [ (n,Rhs mempty (Set.fromList es)) | (n,es) <- eps]

mapGrammar :: (Grammar n t -> Grammar n t') -> GrammarBuilder n t -> GrammarBuilder n t'
mapGrammar f (GrammarBuilder m g) = GrammarBuilder (fmap f m) (f g)

union :: (NonTerminal n, IsGrammar n t)
      => GrammarBuilder n t -> GrammarBuilder n t -> GrammarBuilder n t
union b1 b2 = grammarBuilder $ do
  g1 <- runGrammarBuilder b1
  g2 <- runGrammarBuilder b2
  s <- fresh $ Nothing
  return $ Grammar s $ Map.insert s (mempty {eps = [start g1, start g2]})
                     $ Map.unionWith (error "non terminals are not disjoint") (prods g1) (prods g2)

-- | Inlines all productions reachable via epsilon rules.
epsilonClosure :: (Identifiable n, IsGrammar n t) => GrammarBuilder n t -> GrammarBuilder n t
epsilonClosure = mapGrammar $ \g -> g { prods = Map.mapWithKey (close g) (prods g) }
  where
    close g name _ =
      Rhs { cons = lookup' name g
          , eps = Set.empty
          }

lookup :: (Foldable f, IsGrammar n t) => f n -> Grammar n t -> t n
lookup n g@(Grammar _ p) = cons $ foldMap (\k -> fold (Map.lookup k p)) $ epsilonReachable n g

lookup' :: forall n t. IsGrammar n t => n -> Grammar n t -> t n
lookup' n = lookup ([n] :: [n])

-- Computes the set of non-terminals reachable via epsilon rules.
epsilonReachable :: (Foldable f, IsGrammar n t) => f n -> Grammar n t -> HashSet n
epsilonReachable ns (Grammar _ p) = foldl' go Set.empty ns
  where
    go r n
      | Set.member n r = r
      | otherwise      = foldl' go (Set.insert n r) (eps (fold (Map.lookup n p)))

-- | Removes productions that are not reachable from the start symbol.
dropUnreachable :: (IsGrammar n t) => GrammarBuilder n t -> GrammarBuilder n t
dropUnreachable = mapGrammar $ \g ->
  let reach = reachable' (start g) g
  in g { prods = Map.filterWithKey (\k _ -> Set.member k reach) (prods g) }

reachable :: (Foldable f, IsGrammar n t) => f n -> Grammar n t -> HashSet n
reachable ns g = foldl' go Set.empty ns
  where
    go r n = let rhs = fold (Map.lookup n (prods g))
             in foldl' go (Set.insert n r) ((T.nonTerminals (cons rhs) <> eps rhs) `Set.difference` r)

reachable' :: forall t n. IsGrammar n t => n -> Grammar n t -> HashSet n
reachable' n = reachable ([n] :: [n])

dropUnproductive :: IsGrammar n t => GrammarBuilder n t -> GrammarBuilder n t
dropUnproductive = mapGrammar $ \g ->
  g { prods = Map.mapMaybeWithKey
                  (\k r -> if Set.member k (productive g)
                            then Just (r { cons = T.filter (`Set.member` (productive g)) (cons r)
                                         , eps  = Set.intersection (eps r) (productive g) })
                            else Nothing)
                  (prods g)
    }

-- | Returns all productive nonterminals in the given grammar.
productive :: forall n t. IsGrammar n t => Grammar n t -> HashSet n
productive (Grammar _ prods) = go $ Set.fromList [ n | (n, rhs) <- Map.toList prods, T.productive mempty (cons (rhs))]
  where
    go :: HashSet n -> HashSet n
    go p =
      let p' = Set.union p $ Set.fromList [ n | (n, rhs) <- Map.toList prods, rhsProductive rhs p ]
      in if p == p' then p else go p'

    -- A right hand side is productive if one of its epsilon rules is
    -- productive or a all non-terminals of a constructor are productive.
    rhsProductive :: Rhs n t -> HashSet n -> Bool
    rhsProductive rhs prod = any (`Set.member` prod) (eps rhs)
                          || T.productive prod (cons rhs)

type RenameMap n n' = HashMap (HashSet n) n'
determinize :: forall n n' t. (IsGrammar n t, IsGrammar n' t) => GrammarBuilder n t -> GrammarBuilder n' t
determinize b = grammarBuilder $ state $ \i ->
  let g = getGrammar b
      (s',(i',(ps,_))) = runState (go g [start g]) (i,(mempty,mempty))
  in (Grammar s' ps, i')
 where
   -- go {N1,N2,N3}
   --    [ N1 -> foo(A1,B1) | bar(B1)
   --      N2 -> foo(A2,B2) | bar(B2)
   --      N3 -> foo(A3,B3) | baz(B3)
   --      ...
   --    ]
   -- = [ N1 U N2 U N3 -> foo(A1 U A2 U A3, B1 U B2 U B3) | bar(B1 U B2) | biz(B3) ... ]
   go :: Grammar n t -> HashSet n -> State (Gen n', (ProdMap n' t,RenameMap n n')) n'
   go g ns = do
     rmap <- getRenameMap
     case Map.lookup ns rmap of
       Just x -> return x
       Nothing -> do
         n <- fresh' Nothing
         putRenameMap $ Map.insert ns n rmap
         ctrs <- T.determinize (go g) (lookup ns g)
         result $ Map.insert n (Rhs { cons = ctrs, eps = mempty})
         return n

   getRenameMap :: State (Gen n',(ProdMap n' t',RenameMap n n')) (RenameMap n n')
   getRenameMap = snd . snd <$> get

   putRenameMap :: RenameMap n n' -> State (Gen n',(ProdMap n' t',RenameMap n n')) ()
   putRenameMap r = modify (second (second (const r)))

   result :: (ProdMap n' t -> ProdMap n' t) -> State (Gen n',(ProdMap n' t,RenameMap n n')) ()
   result f = modify (second (first f))

-- | Test whether the first grammar is a subset of the second,
-- i.e. whether L(G1) ⊆ L(G2). The return value is either @Nothing@ in
-- case L(G1) ⊈ L(G2) or a wittness L(G1) ⊆ L(G2).  For example, if
-- @([A,B],X) in subsetOf G1 G2@ then @L(G1(A)) U L(G1(B)) ⊆
-- L(G2(X))@.
subsetOf' :: forall n1 n2 t. (IsGrammar n1 t, IsGrammar n2 t)
         => GrammarBuilder n1 t -> GrammarBuilder n2 t -> Maybe [(HashSet n1,n2)]
b1 `subsetOf'` b2 =
  let g1 = getGrammar b1
      g2 = getGrammar b2
  in fmap (fmap (\(x,y) -> (y,x)) . Map.toList) (execStateT (go g1 g2 [(start g1,start g2)]) mempty)
  where
    go :: Grammar n1 t -> Grammar n2 t -> [(n1,n2)] -> StateT (HashMap n2 (HashSet n1)) Maybe ()
    go g1 g2 l = forM_ l $ \(m1,m2) -> do
      seen <- get
      let xs = Map.lookupDefault mempty m2 seen
      unless (m1 `Set.member` xs) $ do
        put $ Map.insert m2 (Set.insert m1 xs) seen
        T.subsetOf (go g1 g2) (lookup' m1 g1) (lookup' m2 g2)

subsetOf :: (IsGrammar n1 t, IsGrammar n2 t)
        => GrammarBuilder n1 t -> GrammarBuilder n2 t -> Bool
subsetOf m1 m2 = isJust $ subsetOf' m1 m2

toSubterms :: (IsGrammar n t, Monoid (t Int)) => GrammarBuilder n t -> t (GrammarBuilder n t)
toSubterms b =
  let g = getGrammar b
  in T.map (\n -> fromGrammar (g {start = n}))
           (lookup' (start g) g)

fromSubterms :: IsGrammar n t => t (GrammarBuilder n t) -> GrammarBuilder n t
fromSubterms t = grammarBuilder $ do
  s <- fresh Nothing
  (t',p) <- runWriterT $ T.traverse (\b -> do
              g <- lift (runGrammarBuilder b)
              tell (prods g)
              return (start g)
            ) t
  return $ Grammar s $ Map.insert s (Rhs t' mempty) p

instance IsGrammar n t => Eq (GrammarBuilder n t) where
  b1 == b2 = subsetOf b1 b2 && subsetOf b2 b1

instance forall n t. (IsGrammar n t, Monoid (t Int)) => Hashable (GrammarBuilder n t) where
  hashWithSalt s0 b =
    let g = getGrammar (determinize b :: GrammarBuilder Int t)
    in evalState (go g s0 (start g)) mempty
    where
      go :: forall n' t'. IsGrammar n' t' => Grammar n' t' -> Int -> n' -> State (HashSet n') Int
      go g s n = do
        seen <- get
        if Set.member n seen
        then return $ hashWithSalt s n -- Probably wrong. Needs to be 0
        else do
          put $ Set.insert n seen
          T.hashWithSalt (go g) s (lookup' n g)

instance IsGrammar n t => PreOrd (GrammarBuilder n t) where
  (⊑) = subsetOf

instance IsGrammar n t => Complete (GrammarBuilder n t) where
  (⊔) = union


instance (Identifiable n, Semigroup (t n)) => Semigroup (Rhs n t) where
  Rhs c1 e1 <> Rhs c2 e2 = Rhs (c1 <> c2) (e1 <> e2)
instance (Identifiable n, Monoid (t n)) => Monoid (Rhs n t) where
  mappend = (<>)
  mempty = Rhs { cons = mempty, eps = mempty }

instance (Show n, Show (t n)) => Show (GrammarBuilder n t) where
  show b = show (getGrammar b)

instance (Show n, Show (t n)) => Show (Grammar n t) where
  show (Grammar s p) = printf "{start = %s, prods = [%s]}" (show s)
                              (L.intercalate ", " [ printf "%s -> %s" (show n) (show t) | (n,t) <- Map.toList p ])

instance (Show n, Show (t n)) => Show (Rhs n t) where
  show rhs = L.intercalate " | " $ show (cons rhs) : (map show (Set.toList (eps rhs)))

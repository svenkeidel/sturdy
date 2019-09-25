{-# LANGUAGE GADTs #-}
module Data.Utils
  ( maybeHead
  , fromMaybe
  , pow
  , powComplement
  , powComplementPick
  , powComplementN
  , forAll
  , exists
  , module Data.Empty
  , module Data.Singleton
  )
  where

import           Control.Arrow
import           Control.Monad
-- import           Data.Maybe
import qualified Data.IntMap as IM
import           Data.Sequence (Seq)
import           Data.Foldable(toList)
import           Data.Empty
import           Data.Singleton

fromMaybe :: (IsEmpty (f a), IsSingleton (f a), Elem (f a) ~ a) => Maybe a -> f a
fromMaybe (Just a) = singleton a
fromMaybe Nothing  = empty

maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead []    = Nothing

pow :: [a] -> Seq [a]
pow = foldl (\xs x -> fmap (x:) xs<> xs) mempty

-- @powComplement M@ computes for a set M, the set { (X,M\X) | X in P(M) }
powComplement :: [a] -> Seq ([a], [a])
powComplement = foldl (\ys x -> (first (x:) <$> ys) <> (second (x:) <$> ys)) (pure ([],[]))

-- @powComplementN n M@ computes for a set M, the set { [X1,X2,X3] | X1 ⊆ M\∅, X2 ⊆ M\X1, X3 ⊆ M\(X1 ∪ X2) ... }
powComplementN :: Int -> [a] -> Seq [[a]]
powComplementN = go
  where
    go i notSeen
      | i > 1 = do
        (seen',notSeen') <- powComplement notSeen
        rest <- go (i-1) notSeen'
        return (seen':rest)
      | otherwise = return [notSeen]

powComplementPick :: [[a]] -> Seq [[a]]
powComplementPick [] = return []
powComplementPick ([]:_) = return [[]]
powComplementPick l@(x:_) = do
  ixs <- powComplementN (length l) [0..(length x - 1)]
  return $ zipWith (\ix m -> IM.elems (IM.intersection m (IM.fromList [ (i,()) | i <- ix])))
                   ixs elements
  where
    elements = map (IM.fromList . zip [0..]) l

forAll :: (Foldable f, MonadPlus m) => f a -> (a -> m ()) -> m ()
forAll = forM_

exists :: (Foldable f, MonadPlus m) => f a -> (a -> m ()) -> m ()
exists l f = msum [ f x | x <- toList l ]

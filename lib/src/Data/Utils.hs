module Data.Utils where

import           Control.Arrow
import           Data.Maybe
import qualified Data.IntMap as IM
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)

lookupM :: (Ord k, Monoid v) => k -> Map k v -> v
lookupM x m = fromMaybe mempty $ Map.lookup x m

maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead []    = Nothing

pow :: [a] -> Seq [a]
pow = foldl (\xs x -> fmap (x:) xs<> xs) mempty

-- @powComplement M@ computes for a set M, the set { (X,M\X) | X in P(M) }
powComplement :: [a] -> Seq ([a], [a])
powComplement xs = foldl (\ys x -> (first (x:) <$> ys) <> (second (x:) <$> ys)) (pure ([],[])) xs

-- @powComplementN n M@ computes for a set M, the set { [X1,X2,X3] | X1 ⊆ M\∅, X2 ⊆ M\X1, X3 ⊆ M\(X1 ∪ X2) ... }
powComplementN :: Int -> [a] -> Seq [[a]]
powComplementN n xs = go n xs
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
    elements = map IM.fromList (map (zip [0..]) l)

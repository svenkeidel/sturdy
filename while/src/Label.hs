module Label where

import Data.Hashable
import Control.Monad.State

newtype Label = Label Int
  deriving (Show,Ord,Eq)

(@@) :: (Label -> a) -> Int -> a
f @@ i = f (Label i)

instance Hashable Label where
  hashWithSalt salt (Label i) = hashWithSalt salt i

labelAll :: [Label -> a] -> [a]
labelAll = labelAllFrom 1

labelAllFrom :: Int -> [Label -> a] -> [a]
labelAllFrom from = map (\(i,f) -> f (Label i)) . zip [from..]

type Labeled a = State (Int,[a]) ()
runLabeled :: Labeled a -> [a]
runLabeled = snd . flip execState (1,[])

labeled :: (Label -> a) -> Labeled a
labeled f = do
  (i,as) <- get
  let a = f (Label i)
  put (i+1,a:as)

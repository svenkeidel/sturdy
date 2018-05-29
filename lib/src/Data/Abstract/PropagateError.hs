{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Abstract.PropagateError where

import Control.Monad
import Control.Monad.Except
import Data.Abstract.Widening
import Data.Hashable
import Data.Order

import Data.Monoidal

-- | Error is an Either-like type with the special ordering Error ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Error e a = Fail e | Success a
  deriving (Eq, Functor)

instance (Show e,Show a) => Show (Error e a) where
  show (Fail e) = "Error " ++ show e
  show (Success a) = show a

instance (Hashable e, Hashable a) => Hashable (Error e a) where
  hashWithSalt s (Fail e)  = s `hashWithSalt` (0::Int) `hashWithSalt` e
  hashWithSalt s (Success a) = s `hashWithSalt` (1::Int) `hashWithSalt`  a

instance PreOrd a => PreOrd (Error e a) where
  Fail _ ⊑ Success _ = True
  Fail _ ⊑ Fail _ = True
  Success x ⊑ Success y = x ⊑ y
  _ ⊑ _ = False

  Fail _ ≈ Fail _ = True
  Success x ≈ Success y = x ≈ y
  _ ≈ _ = False

instance Complete a => Complete (Error e a) where
  Fail _ ⊔ b = b
  a ⊔ Fail _ = a
  Success x ⊔ Success y = Success (x ⊔ y)

instance UpperBounded a => UpperBounded (Error e a) where
  top = Success top

instance LowerBounded a => LowerBounded (Error e a) where
  bottom = Success bottom

instance Widening a => Widening (Error e a) where
  Fail _ ▽ b = b
  a ▽ Fail _ = a
  Success x ▽ Success y = Success (x ▽ y)

instance MonadError e (Error e) where
  throwError = Fail
  catchError (Fail e) f = f e
  catchError (Success a) _ = Success a

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Monad (Error e) where
  return = Success
  Fail e >>= _ = Fail e
  Success a >>= k = k a

fromError :: a -> Error e a -> a
fromError _ (Success a) = a
fromError a (Fail _) = a

fromEither :: Either e a -> Error e a
fromEither (Left e) = Fail e
fromEither (Right a) = Success a

toEither :: Error e a -> Either e a
toEither (Fail e) = Left e
toEither (Success a) = Right a

fromMaybe :: Maybe a -> Error () a
fromMaybe Nothing = Fail ()
fromMaybe (Just a) = Success a

-- toMaybe :: Error e a -> Maybe a
-- toMaybe (Error _) = Nothing
-- toMaybe (Success a) = Just a


instance Monoidal Error where
  mmap f _ (Fail x) = Fail (f x)
  mmap _ g (Success y) = Success (g y)

  assoc = Iso assocTo assocFrom
    where
      assocTo :: Error a (Error b c) -> Error (Error a b) c
      assocTo (Fail a) = Fail (Fail a)
      assocTo (Success (Fail b)) = Fail (Success b)
      assocTo (Success (Success c)) = Success c

      assocFrom :: Error (Error a b) c -> Error a (Error b c)
      assocFrom (Fail (Fail a)) = Fail a
      assocFrom (Fail (Success b)) = Success (Fail b)
      assocFrom (Success c) = Success (Success c)

instance Symmetric Error where
  commute (Fail a) = Success a
  commute (Success a) = Fail a

instance Strong Error where
  strength (Success a) = pure $ Success a
  strength (Fail a) = Fail <$> a

instance Distributive (,) Error where
  distribute = Iso distTo distFrom
    where
      distTo :: (a,Error b c) -> Error (a,b) (a,c)
      distTo (a,Fail b) = Fail (a,b)
      distTo (a,Success c) = Success (a,c)

      distFrom :: Error (a,b) (a,c) -> (a,Error b c)
      distFrom (Fail (a,b)) = (a,Fail b)
      distFrom (Success (a,c)) = (a,Success c)

instance Distributive Either Error where
  distribute = Iso distTo distFrom
    where
      distTo :: Either a (Error b c) -> Error (Either a b) (Either a c)
      distTo (Left a) = Fail (Left a)
      distTo (Right (Fail b)) = Fail (Right b)
      distTo (Right (Success c)) = Success (Right c)

      distFrom :: Error (Either a b) (Either a c) -> Either a (Error b c)
      distFrom (Fail (Left a)) = Left a
      distFrom (Fail (Right b)) = Right (Fail b)
      distFrom (Success (Left a)) = Left a
      distFrom (Success (Right c)) = Right (Success c)

instance Distributive Error Either where
  distribute = Iso distTo distFrom
    where
      distTo :: Error a (Either b c) -> Either (Error a b) (Error a c)
      distTo (Fail a) = Right (Fail a)
      distTo (Success (Left b)) = Left (Success b)
      distTo (Success (Right c)) = Right (Success c)

      distFrom :: Either (Error a b) (Error a c) -> Error a (Either b c)
      distFrom (Left (Fail a)) = Fail a
      distFrom (Left (Success b)) = Success (Left b)
      distFrom (Right (Fail a)) = Fail a
      distFrom (Right (Success c)) = Success (Right c)

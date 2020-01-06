{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module GenericInterpreter where

import           Prelude hiding (succ, pred, fail, map)
import           Syntax (Expr(..))

import           Control.Arrow
import           Control.Arrow.Utils
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Closure (ArrowClosure)
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Environment (ArrowEnv,ArrowLetRec)
import qualified Control.Arrow.Environment as Env

import           Data.Text (Text)

import           Text.Printf

import           GHC.Exts (IsString(..),Constraint)

-- | Shared interpreter for PCF.
eval :: (ArrowChoice c, ArrowFix (c Expr v), ArrowEnv Text v c, ArrowFail e c, IsString e,
         ArrowClosure Expr v c, ArrowLetRec Text v c, IsVal v c, Env.Join v c, Cls.Join v v c, Join v c)
     => c Expr v
eval = fix $ \ev -> proc e0 -> case e0 of
  Var x _ -> Env.lookup' -< x
  Lam x e l -> Cls.closure -< Lam x e l
  App e1 e2 _ -> do
    fun <- ev -< e1
    arg <- map ev -< e2
    applyClosure' ev -< (fun, arg)
  Zero _ -> zero -< ()
  Succ e _ -> do
    v <- ev -< e
    succ -< v
  Pred e _ -> do
    v <- ev -< e
    pred -< v
  IfZero e1 e2 e3 _ -> do
    v1 <- ev -< e1
    if_ ev ev -< (v1, (e2, e3))
  Let bnds body _ -> do
    vs <- map ev -< [ e | (_,e) <- bnds ]
    Env.letRec ev -< ([ (v,cl) | ((v,_),cl) <- zip bnds vs ], body)
  Apply e _ -> ev -< e
  where
    -- Helper function used to apply closure or a suspended fixpoint computation to its argument.
    applyClosure' ev = Cls.apply $ proc (e,args) -> case e of
      Lam xs body l
        | length xs == length args -> Env.extend' ev -< (zip xs args,Apply body l)
        | otherwise ->
            fail -< fromString $ printf "Try to apply a function with %s parameters to %s arguments"
                                        (show (length xs)) (show (length xs))
      _ -> fail -< fromString $ "Expected a function but got " ++ show e

-- | Interface for numeric operations
class IsVal v c | c -> v where
  type family Join y (c :: * -> * -> *) :: Constraint

  -- | increments the given number value.
  succ :: c v v

  -- | decrements the given number value.
  pred :: c v v

  -- | creates the numeric value zero.
  zero :: c () v

  if_ :: Join z c => c x z -> c y z -> c (v, (x, y)) z

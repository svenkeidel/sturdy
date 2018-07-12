{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PropertySemantics.LiveVariables where

import           Prelude hiding (and,or,not,div,read)

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared
import           ValueSemantics.Abstract
import           ValueSemantics.Unit

import           Data.Text (Text)
import           Data.Label
import           Data.Identifiable
import qualified Data.List as L
import qualified Data.HashMap.Lazy as H
import           Data.HashMap.Lazy (HashMap)

import qualified Data.Abstract.PropagateError as P
import qualified Data.Abstract.Terminating as T
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Store as S
import           Data.Abstract.DiscretePowerset (Pow)
import qualified Data.Abstract.DiscretePowerset as P

import           Control.Arrow.Fix 
import           Control.Arrow.Lift
import           Control.Arrow.Transformer.Abstract.LiveVariables
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

import           GHC.Exts

run :: [Statement] -> [(Statement,Pow Text)]
run stmts =
  L.sortOn (label.fst) $
  S.toList $
  S.map (\((_,(env,st)),v) ->
    case st of
      stmt:_ | stmt `elem` blocks stmts -> do
        -- Extract the transfer function from the value.
        trans <- fst . snd <$> (P.toMaybe =<< T.toMaybe v)
        let -- Extract the addresses from the transfer function
            liveAddresses = vars trans
            -- Extract the live variables by inverting the environment and reading out the live addresses
            liveVars = P.unions $ domain liveAddresses $ invert $ E.toMap env
        Just (stmt,liveVars)
      _ -> Nothing
    ) $
  fst $
  runLeastFixPoint'
    (runInterp
       (runLiveVariables
          (Shared.run :: Fix [Statement] () (LiveVariables Addr (Interp Addr Val (~>))) [Statement] ())))
      (S.empty,(E.empty,stmts))

  where
    invert :: (Identifiable a, Identifiable b) => HashMap a b -> HashMap b (Pow a)
    invert xs = H.fromListWith P.union [ (b,P.singleton a) | (a,b) <- H.toList xs ]
                
    domain :: (Identifiable a, Identifiable b) => Pow a -> HashMap a b -> Pow b
    domain dom f = fromList [ y | x <- toList dom, Just y <- [H.lookup x f] ]

instance (Identifiable v, IsVal val c) => IsVal val (LiveVariables v c) where
  boolLit = lift boolLit
  and = lift and
  or = lift or
  not = lift not
  numLit = lift numLit
  add = lift add
  sub = lift sub
  mul = lift mul
  div = lift div
  eq = lift eq
  lt = lift lt

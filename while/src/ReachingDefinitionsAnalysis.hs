{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module ReachingDefinitionsAnalysis where

-- import           Syntax
-- import qualified GenericInterpreter as Generic
-- import           UnitSemantics

-- import           Data.Text (Text)
-- import           Data.Label
-- import qualified Data.List as L
-- import           Data.HashMap.Lazy (HashMap)
-- import qualified Data.HashMap.Lazy as M

-- import qualified Data.Abstract.Map as SM
-- import qualified Data.Abstract.WeakMap as WM
-- import           Data.Abstract.DiscretePowerset(Pow)
-- import qualified Data.Abstract.StackWidening as SW
-- import qualified Data.Abstract.Widening as W

-- import           Control.Arrow.Fix
-- import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
-- import           Control.Arrow.Transformer.Abstract.Environment
-- import           Control.Arrow.Transformer.Abstract.Store
-- import           Control.Arrow.Transformer.Abstract.Error
-- import           Control.Arrow.Transformer.Abstract.Fix

-- -- | Calculates the entry sets of which definitions may be reached for each statment.
-- run :: [Statement] -> [(Statement,HashMap Text (Pow Label))]
-- run stmts =
--   L.sortOn ((label :: Statement -> Label) . fst) $
--   M.toList $

--   -- Joins the reaching definitions for each statement for all call context.
--   -- Filters out statements created during execution that are not part
--   -- of the input program.
--   M.mapMaybe (\((store,(env,st)),_) -> case st of
--      stmt:_ | stmt `elem` blocks stmts -> Just (stmt, [ (a,c) | (a,b) <- (WM.toList env), Just (_,c) <- [SM.unsafeLookup b store]])
--      _ -> Nothing) $

--   -- get the fixpoint cache
--   fst $

--   -- Run the computation
--   runFixT'' SW.finite W.finite
--     (runErrorT
--       (runStoreT
--         (runReachingDefsT'
--           (runEnvT
--             (runUnitT
--               (Generic.run ::
--                 Fix [Statement] ()
--                  (UnitT
--                    (EnvT Text Addr
--                      (ReachingDefsT Label
--                        (StoreT Addr (Val, Pow Label)
--                          (ErrorT (Pow String)
--                            (FixT _ () () (->))))))) [Statement] ()))))))
--     (SM.empty,(WM.empty,stmts))

-- instance HasLabel (x,[Statement]) Label where
--   label (_,ss) = label (head ss)
-- deriving instance IsVal val c => IsVal val (ReachingDefsT Label c)

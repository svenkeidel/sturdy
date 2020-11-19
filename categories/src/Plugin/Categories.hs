{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.Categories where

import Prelude hiding (lookup,product,(<>))
import GhcPlugins
import Control.Monad

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

toCCC :: (x -> y) -> c x y

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsg (ppr todo)
  let (pre,post) = splitAt 5 todo
  let addRules :: ModGuts -> CoreM ModGuts
      addRules guts = return $ guts { mg_rules = mg_rules guts ++ rewriteRules }
  return (pre ++ [CoreDoPluginPass "Category rewrite rules" addRules] ++ post)
  where
    rewriteRules :: Name -> [CoreRule]
    rewriteRules trigger =
      [ BuiltinRule
        { ru_name = "Category Rewrite"
        , ru_fn = trigger
        , ru_nargs = 4
        , ru_try = \dflags inScope _fn -> 
        }
      ]


data CategoryExpr where
  Primitive :: Var -> CategoryExpr
  Id :: CategoryExpr
  Compose :: CategoryExpr -> CategoryExpr -> CategoryExpr
  Product :: CategoryExpr -> CategoryExpr -> CategoryExpr
  Pi1 :: CategoryExpr
  Pi2 :: CategoryExpr
  CoProduct :: CategoryExpr -> CategoryExpr -> CategoryExpr
  In1 :: CategoryExpr
  In2 :: CategoryExpr
  Distribute :: CategoryExpr -- (A + B) × C -> (A × C) + (B × C)
  Unit :: CategoryExpr
  Apply :: CategoryExpr
  Curry :: CategoryExpr -> CategoryExpr
  Uncurry :: CategoryExpr -> CategoryExpr

(◦) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(◦) = Compose

(△) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(△) = Product

(▽) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(▽) = CoProduct

(×) :: Ctx -> Ctx -> Ctx
(×) = ProductCtx

data Ctx = Variable Var | ProductCtx Ctx Ctx | Empty

data Constrs = Constrs
  { isProduct :: forall a. NamedThing a => a -> Bool
  , isLeft    :: forall a. NamedThing a => a -> Bool
  , isRight   :: forall a. NamedThing a => a -> Bool
  }

rewrite :: Constrs -> Ctx -> CoreExpr -> Maybe CategoryExpr
rewrite constrs ctx e0 = case e0 of
  Var x -> case lookup x ctx of
    Just e' -> return e'
    Nothing -> return (primitive constrs x)
  Lam x e -> do
    f <- rewrite constrs (Variable x × ctx) e
    return (Curry f)
  App e1 e2 -> do
    f <- rewrite constrs ctx e1
    x <- rewrite constrs ctx e2
    return (Apply ◦ (f △ x))
  Case e1 _ _ [(DataAlt ctor, [x,y], e2)] | isProduct constrs ctor -> do
    f <- rewrite constrs ctx e1
    g <- rewrite constrs ((Variable x × Variable y) × ctx) e2
    return (g ◦ (f △ Id))
  Case e1 _ _ [(DataAlt c1, [x], e2), (DataAlt c2, [y], e3)] | isLeft constrs c1 && isRight constrs c2 -> do
    f <- rewrite constrs ctx e1
    g <- rewrite constrs (Variable x × ctx) e2
    h <- rewrite constrs (Variable y × ctx) e3
    return ((g ▽ h) ◦ Distribute ◦ (f △ Id))
  Let (NonRec x e1) e2 ->
    rewrite constrs ctx (App (Lam x e2) e1)
  _ -> Nothing

lookup :: Var -> Ctx -> Maybe CategoryExpr
lookup x (Variable y)
  | x == y    = Just Id
  | otherwise = Nothing
lookup x (ProductCtx ctx1 ctx2) =
  case (lookup x ctx1, lookup x ctx2) of
    (Just f, _) -> return (Pi1 ◦ f)
    (_, Just g) -> return (Pi2 ◦ g)
    (Nothing, Nothing) -> Nothing
lookup _ Empty = Nothing

primitive :: Constrs -> Var -> CategoryExpr
primitive constrs var
  | isProduct constrs var = Curry (Curry ((Pi1 ◦ Pi2) △ Pi1))
  | isLeft constrs var = Curry (In1 ◦ Pi1)
  | isRight constrs var = Curry (In2 ◦ Pi1)
  | otherwise = Primitive var

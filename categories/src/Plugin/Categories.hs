{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Plugin.Categories where

import GhcPlugins
import Data.Maybe(fromJust)

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Hello!"
  return todo

data CategoryExpr where
  Id :: CategoryExpr
  Compose :: CategoryExpr -> CategoryExpr -> CategoryExpr
  Product :: CategoryExpr -> CategoryExpr -> CategoryExpr
  Pi1 :: CategoryExpr
  Pi2 :: CategoryExpr
  Unit :: CategoryExpr
  Apply :: CategoryExpr
  Curry :: CategoryExpr -> CategoryExpr
  Uncurry :: CategoryExpr -> CategoryExpr

(◦) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(◦) = Compose

(△) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(△) = Product

fresh :: Monad m => CoreExpr -> m Var
fresh = undefined

  -- prodName <- thNameToGhcName '(,)
  -- prodCon <- lookupDataCon (fromJust prodName)

rewrite :: DataCon -> CoreExpr -> CoreM CategoryExpr
rewrite prodCon e0 = case e0 of
  (Lam x (Var y)) | x == y ->
    return Id
  (Lam x (App e1 e2)) -> do
    e1' <- rewrite prodCon (Lam x e1)
    e2' <- rewrite prodCon (Lam x e2)
    return (Apply ◦ (e1' △ e2'))
  (Lam x (Lam y e)) -> do
    p <- fresh e0
    rewrite (Lam p (Case (Var p) _ _ [(DataAlt prodCon, [x,y], e)]))
rewrite (Lam x (Case (Var p) _ _ [(DataAlt prodCon, [x,y], e)])) = do

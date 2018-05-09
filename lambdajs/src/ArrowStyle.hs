{-# LANGUAGE Arrows #-}
module ArrowStyle where

import Control.Category
import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as Store

import Syntax
type Store = Map String Value

newtype Arr x y = Arr {runArr :: x -> Store -> Either String (Store, y)}

eval :: Arr Expr Val
eval = proc e -> case e of
    ENumber n -> returnA <- VNumber n
    EId n -> do
        st <- get -< ()
        case Store.lookup n st of
            Just v -> returnA -< v
            Nothing -> throw -< "Variable not in scope"
    

get :: Arr () Store
get = Arr (\() st -> )


            
eval :: Scope -> Expr -> Value
eval _ (ENumber d) = VNumber d
eval _ (EString s) = VString s
eval _ (EBool b) = VBool b
eval _ EUndefined = VUndefined
eval _ ENull = VNull
eval _ (ELambda ids exp) = VLambda ids exp
-- #todo #test fix fields referencing each other(?)
eval s (EObject fields) = 
    VObject (map (\(name, exp) -> (name, eval s exp)) fields)
eval s (EId id) = case get s id of 
    Nothing -> VUndefined
    Just val -> val
eval s (EOp op exps) = evalOp op (map (eval s) exps)
-- #todo fix order of argument evaluation
eval s (EApp (ELambda params body) args) = eval scope body
    where scope = Scope (fromList (zip params (map (eval s) args))) (Just s)
        
    
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.Categories where

import           Prelude hiding (lookup,product,(<>),id,curry,uncurry)
import           GhcPlugins hiding (trace)
import           Control.Monad
import qualified Language.Haskell.TH.Syntax as TH
import qualified Control.CartesianClosedCategory as CCC

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dflags <- getDynFlags
  trigger <- resolveName 'CCC.toCategory

  constrs <- getConstrs

  let rewriteRules :: [CoreRule]
      rewriteRules =
        [ BuiltinRule
          { ru_name = "Category Rewrite"
          , ru_fn = trigger
          , ru_nargs = 0
          , ru_try =
              error "category rewrite rule"
            -- \dflags _ _ exprs -> case exprs of
            --   [Type _, Type _, Type _, expr] -> error "do rewrite" -- trace dflags expr $ rewrite constrs Empty expr
            --   _ -> error "no rewrite" -- Nothing
          }
        ]

      addRule :: ModGuts -> CoreM ModGuts
      addRule guts = do
        putMsg (ppr (mg_rules guts))
        putMsg (ppr rewriteRules)
        return $ guts { mg_rules = rewriteRules ++ mg_rules guts }

      simplify = SimplMode
        { sm_names      = ["Category Rewrite"]
        , sm_phase      = Phase 1
        , sm_rules      = True
        , sm_inline     = True
        , sm_eta_expand = False
        , sm_case_case  = True
        , sm_dflags     = dflags
        }

      removeRule :: ModGuts -> CoreM ModGuts
      removeRule guts = return $ guts {mg_rules = drop (length rewriteRules) (mg_rules guts) }

      passes =
        [ CoreDoPluginPass "Add Rewrite Rule" addRule
        , CoreDoSimplify 7 simplify
        -- Removing the builtin rule is neccessary, because it cannot be
        -- serialized (https://gitlab.haskell.org/ghc/ghc/-/issues/18147)
        , CoreDoPluginPass "Remove Rewrite Rule" removeRule
        ]

      (pre,post) = splitAt 5 todo

  return (pre ++ passes ++ post)

resolveName :: TH.Name -> CoreM Name
resolveName thName = do
  maybeGhcName <- thNameToGhcName thName
  case maybeGhcName of
    Just ghcName -> return ghcName
    Nothing -> do
      errorMsgS ("could not resolve name " ++ show thName)
      mzero

resolveId :: TH.Name -> CoreM Id
resolveId name = resolveName name >>= lookupId

data CategoryExpr where
  Primitive :: Var -> CategoryExpr
  Id :: CategoryExpr
  Compose :: CategoryExpr -> CategoryExpr -> CategoryExpr
  Product :: CategoryExpr -> CategoryExpr -> CategoryExpr
  Pi1 :: CategoryExpr
  Pi2 :: CategoryExpr
  Coproduct :: CategoryExpr -> CategoryExpr -> CategoryExpr
  In1 :: CategoryExpr
  In2 :: CategoryExpr
  Distribute :: CategoryExpr -- (A + B) × C -> (A × C) + (B × C)
  Apply :: CategoryExpr
  Curry :: CategoryExpr -> CategoryExpr
  Uncurry :: CategoryExpr -> CategoryExpr

(◦) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(◦) = Compose

(△) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(△) = Product

(▽) :: CategoryExpr -> CategoryExpr -> CategoryExpr
(▽) = Coproduct

(×) :: Ctx -> Ctx -> Ctx
(×) = ProductCtx

data Ctx = Variable Var | ProductCtx Ctx Ctx | Empty

data Constrs = Constrs
  { isProduct    :: forall a. NamedThing a => a -> Bool
  , isLeft       :: forall a. NamedThing a => a -> Bool
  , isRight      :: forall a. NamedThing a => a -> Bool
  , mkId         :: CoreExpr
  , mkCompose    :: CoreExpr -> CoreExpr -> CoreExpr
  , mkProduct    :: CoreExpr -> CoreExpr -> CoreExpr
  , mkPi1        :: CoreExpr
  , mkPi2        :: CoreExpr
  , mkCoproduct  :: CoreExpr -> CoreExpr -> CoreExpr
  , mkIn1        :: CoreExpr
  , mkIn2        :: CoreExpr
  , mkDistribute :: CoreExpr
  , mkApply      :: CoreExpr
  , mkCurry      :: CoreExpr -> CoreExpr
  , mkUncurry    :: CoreExpr -> CoreExpr
  }

getConstrs :: CoreM Constrs
getConstrs = do
  tuple     <- resolveName '(,)
  left      <- resolveName 'Prelude.Left
  right     <- resolveName 'Prelude.Right
  id        <- resolveId 'CCC.id
  compose   <- resolveId '(CCC..)
  product   <- resolveId '(CCC.&&&)
  p1        <- resolveId 'CCC.pi1
  p2        <- resolveId 'CCC.pi2
  coproduct <- resolveId '(CCC.+++)
  i1        <- resolveId 'CCC.in1
  i2        <- resolveId 'CCC.in2
  dist      <- resolveId 'CCC.distribute1
  app       <- resolveId 'CCC.apply
  curr      <- resolveId 'CCC.curry
  uncurr    <- resolveId 'CCC.uncurry
  return $ Constrs
    { isProduct    = \thing -> getName thing == tuple
    , isLeft       = \thing -> getName thing == left
    , isRight      = \thing -> getName thing == right
    , mkId         = Var id
    , mkCompose    = \e1 e2 -> App (App (Var compose) e1) e2
    , mkProduct    = \e1 e2 -> App (App (Var product) e1) e2
    , mkPi1        = Var p1
    , mkPi2        = Var p2
    , mkCoproduct  = \e1 e2 -> App (App (Var coproduct) e1) e2
    , mkIn1        = Var i1
    , mkIn2        = Var i2
    , mkDistribute = Var dist
    , mkApply      = Var app
    , mkCurry      = \e -> App (Var curr) e
    , mkUncurry    = \e -> App (Var uncurr) e
    }

rewrite :: Constrs -> Ctx -> CoreExpr -> Maybe CoreExpr
rewrite constrs ctx e = do
  catExpr <- toCatExpr constrs ctx e
  return (toCoreExpr constrs catExpr)

toCatExpr :: Constrs -> Ctx -> CoreExpr -> Maybe CategoryExpr
toCatExpr constrs ctx e0 = case e0 of
  Var x -> case lookup x ctx of
    Just e' -> return e'
    Nothing -> return (primitive constrs x)
  Lam x e -> do
    f <- toCatExpr constrs (ctx × Variable x) e
    return (Curry f)
  App e1 e2 -> do
    f <- toCatExpr constrs ctx e1
    x <- toCatExpr constrs ctx e2
    return (Apply ◦ (f △ x))
  Case e1 _ _ [(DataAlt ctor, [x,y], e2)] | isProduct constrs ctor -> do
    f <- toCatExpr constrs ctx e1
    g <- toCatExpr constrs ((Variable x × Variable y) × ctx) e2
    return (g ◦ (Id △ f))
  Case e1 _ _ [(DataAlt c1, [x], e2), (DataAlt c2, [y], e3)] | isLeft constrs c1 && isRight constrs c2 -> do
    f <- toCatExpr constrs ctx e1
    g <- toCatExpr constrs (Variable x × ctx) e2
    h <- toCatExpr constrs (Variable y × ctx) e3
    return ((g ▽ h) ◦ Distribute ◦ (f △ Id))
  Let (NonRec x e1) e2 ->
    toCatExpr constrs ctx (App (Lam x e2) e1)
  _ -> Nothing

primitive :: Constrs -> Var -> CategoryExpr
primitive constrs var
  | isProduct constrs var = Curry (Curry ((Pi2 ◦ Pi1) △ Pi2))
  | isLeft constrs var = Curry (In1 ◦ Pi2)
  | isRight constrs var = Curry (In2 ◦ Pi2)
  | otherwise = Primitive var

lookup :: Var -> Ctx -> Maybe CategoryExpr
lookup x (Variable y)
  | x == y    = Just Id
  | otherwise = Nothing
lookup x (ProductCtx ctx1 ctx2) =
  case (lookup x ctx1, lookup x ctx2) of
    (_, Just g) -> return (Pi2 ◦ g)
    (Just f, _) -> return (Pi1 ◦ f)
    (Nothing, Nothing) -> Nothing
lookup _ Empty = Nothing

toCoreExpr :: Constrs -> CategoryExpr -> CoreExpr
toCoreExpr constrs e0 = case e0 of
  -- Primitive x     -> mkPrimitive constrs x
  Id              -> mkId constrs
  Compose e1 e2   -> mkCompose constrs (toCoreExpr constrs e1) (toCoreExpr constrs e2)
  Product e1 e2   -> mkProduct constrs (toCoreExpr constrs e1) (toCoreExpr constrs e2)
  Pi1             -> mkPi1 constrs
  Pi2             -> mkPi2 constrs
  Coproduct e1 e2 -> mkCoproduct constrs (toCoreExpr constrs e1) (toCoreExpr constrs e2)
  In1             -> mkIn1 constrs
  In2             -> mkIn2 constrs
  Distribute      -> mkDistribute constrs
  Apply           -> mkApply constrs
  Curry e         -> mkCurry constrs (toCoreExpr constrs e)
  Uncurry e       -> mkUncurry constrs (toCoreExpr constrs e)

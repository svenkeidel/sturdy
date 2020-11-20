{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.Categories where

import           Prelude hiding (lookup,product,(<>),id)
import           GhcPlugins hiding (trace)
import           Control.Monad
import qualified Control.Category
import qualified Language.Haskell.TH.Syntax as TH
import qualified Debug.Trace as Debug
import GHC (lookupGlobalName)

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

trace :: Outputable a => DynFlags -> a -> b -> b
trace dflags str b = Debug.trace (show (runSDoc (ppr str) (initSDocContext dflags (defaultUserStyle dflags)))) b

toCategory :: (x -> y) -> c x y
toCategory = undefined
{-# NOINLINE toCategory #-}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dflags <- getDynFlags
  trigger <- resolveName 'Plugin.Categories.toCategory

  constrs <- getConstrs

  let rewriteRules :: [CoreRule]
      rewriteRules =
        [ BuiltinRule
          { ru_name = "Category Rewrite"
          , ru_fn = trigger
          , ru_nargs = 4
          , ru_try =
              error "category rewrite rule"
            -- \dflags _ _ exprs -> case exprs of
            --   [Type _, Type _, Type _, expr] -> error "do rewrite" -- trace dflags expr $ rewrite constrs Empty expr
            --   _ -> error "no rewrite" -- Nothing
          }
        ]

      addRule :: ModGuts -> CoreM ModGuts
      addRule guts = return $ guts { mg_rules = rewriteRules ++ mg_rules guts }

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



getConstrs :: CoreM Constrs
getConstrs = do
  product <- resolveName '(,)
  left <- resolveName 'Prelude.Left
  right <- resolveName 'Prelude.Right
  id <- resolveId 'Control.Category.id
  return $ Constrs
    { isProduct = \thing -> getName thing == product
    , isLeft = \thing -> getName thing == left
    , isRight = \thing -> getName thing == right
    , mkId = Var id
    }

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
  , mkId      :: CoreExpr
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
    f <- toCatExpr constrs (Variable x × ctx) e
    return (Curry f)
  App e1 e2 -> do
    f <- toCatExpr constrs ctx e1
    x <- toCatExpr constrs ctx e2
    return (Apply ◦ (f △ x))
  Case e1 _ _ [(DataAlt ctor, [x,y], e2)] | isProduct constrs ctor -> do
    f <- toCatExpr constrs ctx e1
    g <- toCatExpr constrs ((Variable x × Variable y) × ctx) e2
    return (g ◦ (f △ Id))
  Case e1 _ _ [(DataAlt c1, [x], e2), (DataAlt c2, [y], e3)] | isLeft constrs c1 && isRight constrs c2 -> do
    f <- toCatExpr constrs ctx e1
    g <- toCatExpr constrs (Variable x × ctx) e2
    h <- toCatExpr constrs (Variable y × ctx) e3
    return ((g ▽ h) ◦ Distribute ◦ (f △ Id))
  Let (NonRec x e1) e2 ->
    toCatExpr constrs ctx (App (Lam x e2) e1)
  _ -> Nothing

toCoreExpr :: Constrs -> CategoryExpr -> CoreExpr
toCoreExpr constrs e0 = case e0 of
  Id -> mkId constrs
  _ -> error "could not translate expression"

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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
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

data CCC = CCC
  { cccType :: Type
  , categoryDict :: CoreExpr
  , cartesianDict :: CoreExpr
  , cocartesianDict :: CoreExpr
  , closedDict :: CoreExpr
  , distributiveDict :: CoreExpr
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dflags <- getDynFlags
  trigger <- resolveName 'CCC.toCategory

  ctrs <- getConstrs


  let rewriteRules :: [CoreRule]
      rewriteRules =
        [ BuiltinRule
          { ru_name = "Category Rewrite"
          , ru_fn = trigger
          , ru_nargs = 5
          , ru_try =
              \_ _ _ exprs -> case exprs of
                [Type c, Type _, Type _, dict, expr] ->
                  -- pprPanic "" (ppr (collectArgs dict)) $
                  let (_,[_,_,_,_,_,
                          catDict,cartDict,cocartDict,closedDict,distribDict]) = collectArgs dict in
                  let ?constrs = ctrs in
                  let ?ccc = CCC
                        { cccType = c
                        , categoryDict = catDict
                        , cartesianDict = cartDict
                        , cocartesianDict = cocartDict
                        , closedDict = closedDict
                        , distributiveDict = distribDict
                        }
                  in
                  rewrite Empty expr
                _ -> Nothing
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
        -- , CoreDoRuleCheck InitialPhase "Category Rewrite"
        , CoreDoSimplify 7 simplify
        -- Removing the builtin rule is neccessary, because it cannot be
        -- serialized (https://gitlab.haskell.org/ghc/ghc/-/issues/18147)
        , CoreDoPluginPass "Remove Rewrite Rule" removeRule
        ]

      (pre,post) = splitAt 5 todo
      -- (pre,post) = splitAt 5 todo

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

resolveTyCon :: TH.Name -> CoreM TyCon
resolveTyCon name = resolveName name >>= lookupTyCon

data CategoryExpr where
  Primitive :: Type -> Type -> Var -> CategoryExpr
  Id :: Type -> CategoryExpr
  Compose :: Type -> Type -> Type -> CategoryExpr -> CategoryExpr -> CategoryExpr
  Product :: Type -> Type -> Type -> CategoryExpr -> CategoryExpr -> CategoryExpr
  Pi1 :: Type -> Type -> CategoryExpr
  Pi2 :: Type -> Type -> CategoryExpr
  Coproduct :: Type -> Type -> Type -> CategoryExpr -> CategoryExpr -> CategoryExpr
  In1 :: Type -> Type -> CategoryExpr
  In2 :: Type -> Type -> CategoryExpr
  Distribute :: Type -> Type -> Type -> CategoryExpr -- (A + B) × C -> (A × C) + (B × C)
  Apply :: Type -> Type -> CategoryExpr
  Curry :: Type -> Type -> Type -> CategoryExpr -> CategoryExpr
  Uncurry :: Type -> Type -> Type -> CategoryExpr -> CategoryExpr

(◦) :: (HasCallStack, ?constrs :: Constrs) => CategoryExpr -> CategoryExpr -> CategoryExpr
f ◦ g = Compose (dom g) (cod g) (cod f) f g

(△) :: (HasCallStack, ?constrs :: Constrs) => CategoryExpr -> CategoryExpr -> CategoryExpr
f △ g = Product (dom f) (cod f) (cod g) f g

(▽) :: (HasCallStack, ?constrs :: Constrs) => CategoryExpr -> CategoryExpr -> CategoryExpr
f ▽ g = Coproduct (dom f) (dom g) (cod f) f g

curry :: (HasCallStack, ?constrs :: Constrs) => CategoryExpr -> CategoryExpr
curry f = case splitTyConApp_maybe (dom f) of
  Just (_,[x,y]) -> Curry x y (cod f) f
  _ -> pprPanic "" ("expected a pair type when constructing curry but got" <> ppr (dom f))

uncurry :: (HasCallStack, ?constrs :: Constrs) => CategoryExpr -> CategoryExpr
uncurry f = case splitFunTy_maybe (cod f) of
  Just (y,z) -> Uncurry (dom f) y z f
  _ -> pprPanic "" ("expected a function type when constructing uncurry but got" <> ppr (cod f))

dom :: (?constrs :: Constrs) => CategoryExpr -> Type
dom e0 = case e0 of
  Primitive x _ _ -> x
  Id x -> x
  Compose x _ _ _ _ -> x
  Product x _ _ _ _ -> x
  Pi1 x y -> mkTuple ?constrs x y
  Pi2 x y -> mkTuple ?constrs x y
  Coproduct x y _ _ _ -> mkEither ?constrs x y
  In1 x _ -> x
  In2 _ y -> y
  Distribute x y z -> mkTuple ?constrs x (mkEither ?constrs y z)
  Apply x y -> mkTuple ?constrs (mkFun ?constrs x y) x
  Curry x _ _ _ -> x
  Uncurry x y _ _ -> mkTuple ?constrs x y

cod :: (?constrs :: Constrs) => CategoryExpr -> Type
cod e0 = case e0 of
  Primitive _ y _ -> y
  Id x -> x
  Compose _ _ z _ _ -> z
  Product _ y z _ _ -> mkTuple ?constrs y z
  Pi1 x _ -> x
  Pi2 _ y -> y
  Coproduct _ _ z _ _ -> z
  In1 x y -> mkEither ?constrs x y
  In2 x y -> mkEither ?constrs x y
  Distribute x y z -> mkEither ?constrs (mkTuple ?constrs x y) (mkTuple ?constrs x z)
  Apply _ y -> y
  Curry _ y z _ -> mkFun ?constrs y z
  Uncurry _ _ z _ -> z

(×) :: Ctx -> Ctx -> Ctx
ctx1 × ctx2 = ProductCtx (mkBoxedTupleTy [ctxType ctx1, ctxType ctx2]) ctx1 ctx2

data Ctx = Variable Var | ProductCtx Type Ctx Ctx | Empty

ctxType :: Ctx -> Type
ctxType ctx0 = case ctx0 of
  Variable x -> varType x
  ProductCtx t _ _ -> t
  Empty -> mkBoxedTupleTy []

data Constrs = Constrs
  { isProduct    :: forall a. NamedThing a => a -> Bool
  , isLeft       :: forall a. NamedThing a => a -> Bool
  , isRight      :: forall a. NamedThing a => a -> Bool
  , mkId         :: (?ccc :: CCC) => Type -> CoreExpr
  , mkCompose    :: (?ccc :: CCC) => Type -> Type -> Type -> CoreExpr -> CoreExpr -> CoreExpr
  , mkProduct    :: (?ccc :: CCC) => Type -> Type -> Type -> CoreExpr -> CoreExpr -> CoreExpr
  , mkPi1        :: (?ccc :: CCC) => Type -> Type -> CoreExpr
  , mkPi2        :: (?ccc :: CCC) => Type -> Type -> CoreExpr
  , mkCoproduct  :: (?ccc :: CCC) => Type -> Type -> Type -> CoreExpr -> CoreExpr -> CoreExpr
  , mkIn1        :: (?ccc :: CCC) => Type -> Type -> CoreExpr
  , mkIn2        :: (?ccc :: CCC) => Type -> Type -> CoreExpr
  , mkDistribute :: (?ccc :: CCC) => Type -> Type -> Type -> CoreExpr
  , mkApply      :: (?ccc :: CCC) => Type -> Type -> CoreExpr
  , mkCurry      :: (?ccc :: CCC) => Type -> Type -> Type -> CoreExpr -> CoreExpr
  , mkUncurry    :: (?ccc :: CCC) => Type -> Type -> Type -> CoreExpr -> CoreExpr
  , mkFun        :: Type -> Type -> Type
  , mkEither     :: Type -> Type -> Type
  , mkTuple      :: Type -> Type -> Type
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
  eitherTy  <- resolveTyCon ''Either
  return $ Constrs
    { isProduct    = \thing -> getName thing == tuple
    , isLeft       = \thing -> getName thing == left
    , isRight      = \thing -> getName thing == right
    , mkId         = \x -> op id [x] (categoryDict ?ccc)
    , mkCompose    = \x y z e1 e2 -> App (App (op compose [y,z,x] (categoryDict ?ccc)) e1) e2
    , mkProduct    = \x y z e1 e2 -> App (App (op product [x,y,z] (cartesianDict ?ccc)) e1) e2
    , mkPi1        = \x y -> op p1 [x,y] (cartesianDict ?ccc)
    , mkPi2        = \x y -> op p2 [x,y] (cartesianDict ?ccc)
    , mkCoproduct  = \x y z e1 e2 -> App (App (op coproduct [x,y,z] (cocartesianDict ?ccc)) e1) e2
    , mkIn1        = \x y -> op i1 [x,y] (cocartesianDict ?ccc)
    , mkIn2        = \x y -> op i2 [x,y] (cocartesianDict ?ccc)
    , mkDistribute = \x y z -> op dist [x,y,z] (distributiveDict ?ccc)
    , mkApply      = \x y -> op app [x,y] (closedDict ?ccc)
    , mkCurry      = \x y z e -> App (op curr [x,y,z] (closedDict ?ccc)) e
    , mkUncurry    = \x y z e -> App (op uncurr [x,y,z] (closedDict ?ccc)) e
    , mkFun        = \x y -> mkFunTy x y
    , mkTuple      = \x y -> mkBoxedTupleTy [x,y]
    , mkEither     = \x y -> mkTyConApp eitherTy [x,y]
    }
  where
    op v ts dict = App (mkTyApps (Var v) (cccType ?ccc :ts)) dict

rewrite :: (HasCallStack, ?constrs :: Constrs, ?ccc :: CCC) => Ctx -> CoreExpr -> Maybe CoreExpr
rewrite ctx e = do
  catExpr <- toCatExpr ctx e
  return (toCoreExpr catExpr)

toCatExpr :: (HasCallStack, ?constrs :: Constrs) => Ctx -> CoreExpr -> Maybe CategoryExpr
toCatExpr ctx e0 = case e0 of
  Var x -> case lookup x ctx of
    Just e' -> return e'
    Nothing -> return (primitive x)
  Lam x e -> do
    f <- toCatExpr (ctx × Variable x) e
    return (curry f)
  App e1 e2 -> do
    f <- toCatExpr ctx e1
    x <- toCatExpr ctx e2
    let (tx,ty) = splitFunTy (cod f)
    return (Apply tx ty ◦ (f △ x))
  Case e1 _ _ [(DataAlt ctor, [x,y], e2)] | isProduct ?constrs ctor -> do
    f <- toCatExpr ctx e1
    g <- toCatExpr ((Variable x × Variable y) × ctx) e2
    return (g ◦ (Id (ctxType ctx) △ f))
  Case e1 _ _ [(DataAlt c1, [x], e2), (DataAlt c2, [y], e3)] | isLeft ?constrs c1 && isRight ?constrs c2 -> do
    f <- toCatExpr ctx e1
    g <- toCatExpr (Variable x × ctx) e2
    h <- toCatExpr (Variable y × ctx) e3
    let gamma = ctxType ctx
        (_, [tx,ty]) = splitTyConApp (cod f)
    return ((g ▽ h) ◦ Distribute gamma tx ty ◦ (Id gamma △ f))
  Let (NonRec x e1) e2 ->
    toCatExpr ctx (App (Lam x e2) e1)
  _ -> Nothing

primitive :: (HasCallStack, ?constrs :: Constrs) => Var -> CategoryExpr
primitive var
  | isProduct ?constrs var = undefined -- curry (curry ((Pi2 _ _ ◦ Pi1 _ _) △ Pi2 _ _))
  | isLeft ?constrs var = undefined -- curry (In1 _ _ ◦ Pi2 _ _)
  | isRight ?constrs var = undefined -- curry (In2 _ _ ◦ Pi2 _ _)
  | isFunTy (varType var) = let (x,y) = splitFunTy (varType var) in Primitive x y var
  | otherwise = error "do not recoginize primitive"

lookup :: (HasCallStack, ?constrs :: Constrs) => Var -> Ctx -> Maybe CategoryExpr
lookup x (Variable y)
  | x == y    = Just (Id (varType y))
  | otherwise = Nothing
lookup x (ProductCtx gamma ctx1 ctx2) =
  case (lookup x ctx1, lookup x ctx2) of
    (_, Just g) -> return (g ◦ Pi2 (ctxType ctx1) (ctxType ctx2))
    (Just f, _) -> return (f ◦ Pi1 (ctxType ctx1) (ctxType ctx2))
    (Nothing, Nothing) -> Nothing
lookup _ Empty = Nothing

toCoreExpr :: (HasCallStack, ?constrs :: Constrs, ?ccc :: CCC) => CategoryExpr -> CoreExpr
toCoreExpr e0 = case e0 of
  -- Primitive x     -> mkPrimitive constrs x
  Id x                  -> mkId ?constrs x
  Compose x y z e1 e2   -> mkCompose ?constrs x y z (toCoreExpr e1) (toCoreExpr e2)
  Product x y z e1 e2   -> mkProduct ?constrs x y z (toCoreExpr e1) (toCoreExpr e2)
  Pi1 x y               -> mkPi1 ?constrs x y
  Pi2 x y               -> mkPi2 ?constrs x y
  Coproduct x y z e1 e2 -> mkCoproduct ?constrs x y z (toCoreExpr e1) (toCoreExpr e2)
  In1 x y               -> mkIn1 ?constrs x y
  In2 x y               -> mkIn2 ?constrs x y
  Distribute x y z      -> mkDistribute ?constrs x y z
  Apply x y             -> mkApply ?constrs x y
  Curry x y z e         -> mkCurry ?constrs x y z (toCoreExpr e)
  Uncurry x y z e       -> mkUncurry ?constrs x y z (toCoreExpr e)

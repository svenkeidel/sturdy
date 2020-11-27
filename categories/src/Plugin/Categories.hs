{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Plugin.Categories where

import           Prelude hiding (lookup,product,(<>),id,curry,uncurry)
import           GhcPlugins hiding (trace)
import           Class
import           Control.Monad
import qualified Language.Haskell.TH.Syntax as TH
import qualified Control.CartesianClosedCategory as CCC
import           Data.List (find)

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
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
          , ru_nargs = 11
          , ru_try =
              \_ _ _ exprs ->
                case exprs of
                [Type _prim, Type c, Type _x, Type _y, primDict, catDict,cartDict,cocartDict, closDict, distribDict, Lam x e] ->
                  let ?constrs = ctrs in
                  let (primClass, _) = getClassPredTys (exprType primDict) in
                  let ?ccc = CCC
                        { cccType = c
                        , primitiveDict = primDict
                        , primitiveClass = primClass
                        , categoryDict = catDict
                        , cartesianDict = cartDict
                        , cocartesianDict = cocartDict
                        , closedDict = closDict
                        , distributiveDict = distribDict
                        } in
                  let e' = rewrite (Variable x) e
                  in pprTrace "Category Rewrite" (vcat ["before: " <> ppr (Lam x e),
                                                        "after:  " <> ppr e']) $ return $ e'
                _ -> pprPanic "Category Rewrite" ("Could not compile function: " <> ppr exprs)
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
  , mkPrimitive  :: (?ccc :: CCC) => Type -> Type -> Var -> CoreExpr
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

data CCC = CCC
  { cccType :: Type
  , primitiveDict :: CoreExpr
  , primitiveClass :: Class
  , categoryDict :: CoreExpr
  , cartesianDict :: CoreExpr
  , cocartesianDict :: CoreExpr
  , closedDict :: CoreExpr
  , distributiveDict :: CoreExpr
  }

getConstrs :: CoreM Constrs
getConstrs = do
  tuple     <- resolveId '(,)
  left      <- resolveId 'Prelude.Left
  right     <- resolveId 'Prelude.Right
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
    { isProduct    = \thing -> getOccName thing == getOccName tuple
    , isLeft       = \thing -> getOccName thing == getOccName left
    , isRight      = \thing -> getOccName thing == getOccName right
    , mkPrimitive  = \x y v ->
        let var = lookupPrimVar v
            c = tyConAppTyCon (cccType ?ccc)
            ty = mkTyConApp c [x, y]
            expr = Var (setVarType var ty)
            -- expr = op var [Type (cccType ?ccc), primitiveDict ?ccc, Type x, Type y]
        in pprTrace "mkPrimitive" (vcat [ "dom: " <> ppr x
                                        , "cod: " <> ppr y
                                        , "var: " <> ppr var <> " :: " <> ppr (varType var)
                                        , "expr: " <> ppr expr <> " :: " <> ppr (exprType expr)
                                        ]) $ expr
    , mkId         = \x -> op id [Type (typeKind x), Type (cccType ?ccc), categoryDict ?ccc, Type x]
    , mkCompose    = \x y z e1 e2 ->
        let tys = [Type (typeKind x), Type (cccType ?ccc), categoryDict ?ccc, Type y, Type z, Type x] in
        mkApps (op compose tys) [e1,e2]
    , mkProduct    = \x y z e1 e2 ->
        let tys = [Type (cccType ?ccc), cartesianDict ?ccc, Type x, Type y, Type z] in
        mkApps (op product tys) [e1,e2]
    , mkPi1        = \x y -> op p1 [Type (cccType ?ccc), cartesianDict ?ccc, Type x, Type y]
    , mkPi2        = \x y -> op p2 [Type (cccType ?ccc), cartesianDict ?ccc, Type x, Type y]
    , mkCoproduct  = \x y z e1 e2 ->
        let tys = [Type (cccType ?ccc), cocartesianDict ?ccc, Type x, Type y, Type z] in
        mkApps (op coproduct tys) [e1,e2]
    , mkIn1        = \x y -> op i1 [Type (cccType ?ccc), cocartesianDict ?ccc, Type x, Type y]
    , mkIn2        = \x y -> op i2 [Type (cccType ?ccc), cocartesianDict ?ccc, Type x, Type y]
    , mkDistribute = \x y z -> op dist [Type (cccType ?ccc), distributiveDict ?ccc, Type x, Type y, Type z]
    , mkApply      = \x y -> op app [Type (cccType ?ccc), closedDict ?ccc, Type x, Type y]
    , mkCurry      = \x y z e ->
        let tys = [Type (cccType ?ccc), closedDict ?ccc, Type x, Type y, Type z] in
        mkApps (op curr tys) [e]
    , mkUncurry    = \x y z e ->
        let tys = [Type (cccType ?ccc), closedDict ?ccc, Type x, Type y, Type z] in
        mkApps (op uncurr tys) [e]
    , mkFun        = \x y -> mkFunTy x y
    , mkTuple      = \x y -> mkBoxedTupleTy [x,y]
    , mkEither     = \x y -> mkTyConApp eitherTy [x,y]
    }
  where
    op :: HasCallStack => Var -> [CoreExpr] -> CoreExpr
    op v ts =
      let ty = applyTypeToArgs (Var v) (varType v) ts
      in Var (setVarType v ty)

lookupPrimVar :: (HasCallStack, ?ccc :: CCC) => Var -> Var
lookupPrimVar v = case findMethod (convertPrimVar (getOccName v)) methods of
  Just method -> method
  Nothing -> pprPanic "convertPrimVar" ("could not find primitive method" <+> ppr v)
  where
    methods = classMethods (primitiveClass ?ccc)
    convertPrimVar var
      | var == mkVarOcc "(,)" = mkVarOcc "tuple"
      | otherwise = var
      -- | var == mkVarOcc "Left" = mkVarOcc "left"
      -- | var == mkVarOcc "Right" = mkVarOcc "right"

findMethod :: OccName -> [Id] -> Maybe Id
findMethod name = find (\v' -> name == getOccName v')

rewrite :: (HasCallStack, ?constrs :: Constrs, ?ccc :: CCC) => Ctx -> CoreExpr -> CoreExpr
rewrite ctx e = toCoreExpr (toCatExpr ctx e)

applyType :: HasCallStack => Type -> CategoryExpr -> CategoryExpr
applyType t (Primitive x y var) =
  -- pprTrace "applyType" (ppr t' <> " [" <> ppr t <>"] = " <> ppr (applyTypeToArgs (Var var) t' [Type t])) $
  Primitive x (applyTypeToArgs (Var var) y [Type t]) var
applyType t expr = pprTrace "applyType" ("couldn't apply type " <> ppr t <> " to expression") $ expr

toCatExpr :: (HasCallStack, ?constrs :: Constrs) => Ctx -> CoreExpr -> CategoryExpr
toCatExpr ctx e0 = case e0 of
  Var x -> case lookup x ctx of
    Just e' -> e'
    Nothing -> Primitive (ctxType ctx) (varType x) x
  Lam x e ->
    let f = toCatExpr (ctx × Variable x) e
    in curry f
  App e1 (Type t) -> applyType t $ toCatExpr ctx e1
  App e1 e2 ->
    let f = toCatExpr ctx e1
        x = toCatExpr ctx e2
        (tx,ty) = splitFunTy (cod f)
    in Apply tx ty ◦ (f △ x)
  Case e1 _ _ [(DataAlt ctor, [x,y], e2)] | isProduct ?constrs ctor ->
    let f = toCatExpr ctx e1
        g = toCatExpr ((Variable x × Variable y) × ctx) e2
    in g ◦ (Id (ctxType ctx) △ f)
  Case e1 _ _ [(DataAlt c1, [x], e2), (DataAlt c2, [y], e3)] | isLeft ?constrs c1 && isRight ?constrs c2 ->
    let f = toCatExpr ctx e1
        g = toCatExpr (Variable x × ctx) e2
        h = toCatExpr (Variable y × ctx) e3
        gamma = ctxType ctx
        (_, [tx,ty]) = splitTyConApp (cod f)
    in (g ▽ h) ◦ Distribute gamma tx ty ◦ (Id gamma △ f)
  Let (NonRec x e1) e2 ->
    toCatExpr ctx (App (Lam x e2) e1)
  _ -> pprPanic "toCatExpr" $ "Could not compile expression " <> ppr e0

lookup :: (HasCallStack, ?constrs :: Constrs) => Var -> Ctx -> Maybe CategoryExpr
lookup x (Variable y)
  | x == y    = Just (Id (varType y))
  | otherwise = Nothing
lookup x (ProductCtx _ ctx1 ctx2) =
  case (lookup x ctx1, lookup x ctx2) of
    (_, Just g) -> return (g ◦ Pi2 (ctxType ctx1) (ctxType ctx2))
    (Just f, _) -> return (f ◦ Pi1 (ctxType ctx1) (ctxType ctx2))
    (Nothing, Nothing) -> Nothing
lookup _ Empty = Nothing

toCoreExpr :: (HasCallStack, ?constrs :: Constrs, ?ccc :: CCC) => CategoryExpr -> CoreExpr
toCoreExpr e0 = case e0 of
  Primitive x y v       -> mkPrimitive ?constrs x y v
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

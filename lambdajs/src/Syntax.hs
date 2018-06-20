{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Syntax where

import           Data.Abstract.Environment
import           Data.Hashable
import           Data.List                 (sort)
import           Data.Set
import           GHC.Generics              (Generic)

type Ident = String
data Label = Label String
  deriving (Ord, Eq, Show, Generic)
deriving instance Hashable Label

data Location = Location Int
  deriving (Ord, Eq, Show, Generic)
deriving instance Hashable Location

data Op
    = ONumPlus
    | OStrPlus
    | OMul | ODiv | OMod | OSub
    | OLt  | OStrLt
    | OBAnd | OBOr | OBXOr | OBNot
    | OLShift | OSpRShift | OZfRShift
    | OStrictEq
    | OAbstractEq
    | OTypeof
    | OSurfaceTypeof -- not implemented
    | OPrimToNum
    | OPrimToStr
    | OPrimToBool
    | OIsPrim
    | OHasOwnProp
    | OToInteger | OToInt32 | OToUInt32
    | OPrint -- ^for Rhino -- not implemented
    | OStrContains | OStrSplitRegExp | OStrSplitStrExp -- ^for Regexes -- not implemented
    | OStrStartsWith -- ^for forin
    | OStrLen
    | OObjIterHasNext | OObjIterNext | OObjIterKey -- ^more forin -- not implemented
    | OObjCanDelete -- not implemented
    | OMathExp | OMathLog | OMathCos | OMathSin | OMathAbs | OMathPow
    | ORegExpMatch | ORegExpQuote -- not implemented
    deriving (Show, Eq, Generic, Ord)
instance Hashable Op

data Expr
    = ENumber Double
    | EString String
    | EBool Bool
    | EUndefined
    | ENull
    | ELambda [Ident] Expr
    | EObject [(String, Expr)]
    | EId Ident
    | EOp Op [Expr]
    | EApp Expr [Expr]
    | ELet [(Ident, Expr)] Expr
    | ESetRef Expr Expr
    | ERef Expr
    | EDeref Expr
    | EGetField Expr Expr
    | EUpdateField Expr Expr Expr
    | EDeleteField Expr Expr
    | ESeq Expr Expr
    | EIf Expr Expr Expr
    | EWhile Expr Expr
    | ELabel Label Expr
    | EBreak Label Expr
    | EThrow Expr
    | ECatch Expr Expr
    | EFinally Expr Expr
    -- An expression that calls eval, or a related function. If EEval becomes the active expression,
    -- our model immediately aborts.
    | EEval
    deriving (Show, Eq, Generic, Ord)
instance Hashable Expr

data Type
    = TNumber
    | TString
    | TBool
    | TUndefined
    | TNull
    | TLambda [Ident] Expr (Env Ident Type')
    | TObject [(Ident, Type')]
    | TTop
    | TBottom
    | TRef Location'
    | TThrown Type'
    | TBreak Label Type'
    deriving (Show, Eq, Generic, Ord)
deriving instance Hashable Type

instance Ord (Env Ident Type') where
    (<=) a b = (sort $ Data.Abstract.Environment.toList a) <= (sort $ Data.Abstract.Environment.toList b)

instance (Hashable v, Ord v) => Hashable (Set v) where
  hashWithSalt salt set =
    Prelude.foldr (\x s -> s + (hash x)) salt (sort $ Data.Set.toList set)

type Location' = Set Location
instance Generic Location'

type Type' = Set Type
instance Generic Type'

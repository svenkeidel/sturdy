{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

import Data.Hashable

import Control.Monad

import Test.QuickCheck

data CompilationUnit = CompilationUnit { fileModifiers :: [Modifier]
                                       , fileType :: FileType
                                       , fileName :: String
                                       , extends :: Maybe String
                                       , implements :: [String]
                                       , fileBody :: [Member]
                                       }

instance Eq CompilationUnit where
  (==) f1 f2 = fileName f1 == fileName f2

instance Show CompilationUnit where
  show f = "CompilationUnit {"
           ++ show (fileModifiers f) ++ " "
           ++ show (fileType f) ++ " "
           ++ show (fileName f) ++ " "
           ++ ex
           ++ impl
           ++ "}"
    where
      ex = case extends f of
        Just c -> "extends " ++ show c
        Nothing -> ""
      impl = case length (implements f) of
        0 -> ""
        xs -> "implements " ++ show xs

data Modifier
  = Abstract
  | Final
  | Native
  | Public
  | Protected
  | Private
  | Static
  | Synchronized
  | Transient
  | Volatile
  | Strictfp
  | Enum
  | Annotation deriving (Eq)

instance Show Modifier where
  show Abstract     = "abstract"
  show Final        = "final"
  show Native       = "native"
  show Public       = "public"
  show Protected    = "protected"
  show Private      = "private"
  show Static       = "static"
  show Synchronized = "synchronized"
  show Transient    = "transient"
  show Volatile     = "volatile"
  show Strictfp     = "strictfp"
  show Enum         = "enum"
  show Annotation   = "annotation"

data FileType
  = ClassFile
  | InterfaceFile

instance Show FileType where
  show ClassFile = "class"
  show InterfaceFile = "interface"

data MethodSignature = MethodSignature String Type String [Type] deriving (Show, Eq)

data UnnamedMethodSignature = UnnamedMethodSignature Type [Type] deriving (Show, Eq)

data FieldSignature = FieldSignature String Type String deriving (Eq)

instance Ord FieldSignature where
  compare (FieldSignature c1 _ n1) (FieldSignature c2 _ n2) =
    case compare c1 c2 of
      LT -> LT
      EQ -> compare n1 n2
      GT -> GT

instance Show FieldSignature where
  show (FieldSignature c t n) =
    "<" ++ show c ++ ": " ++ show t ++ " " ++ show n ++ ">"

instance Hashable FieldSignature where
  hashWithSalt s (FieldSignature c t n) = s + hash c + hash t + hash n

data Member = FieldMember Field | MethodMember Method

data Field = Field { fieldModifiers :: [Modifier]
                   , fieldType :: Type
                   , fieldName :: String
                   }

fieldSignature :: CompilationUnit -> Field -> FieldSignature
fieldSignature CompilationUnit{fileName=c} Field{fieldType=t,fieldName=n}
  = FieldSignature c t n

data Method = Method { methodModifiers :: [Modifier]
                     , returnType :: Type
                     , methodName :: String
                     , parameters :: [Type]
                     , throws :: [String]
                     , methodBody :: MethodBody
                     } deriving (Show, Eq)

methodSignature :: CompilationUnit -> Method -> MethodSignature
methodSignature CompilationUnit{fileName=c} Method{returnType=t,parameters=p,methodName=n}
  = MethodSignature c t n p

data Type
  = ArrayType Type
  | BooleanType
  | ByteType
  | CharType
  | DoubleType
  | FloatType
  | IntType
  | LongType
  | NullType
  | RefType String
  | ShortType
  | UnknownType
  | VoidType deriving (Eq)

instance Show Type where
  show (ArrayType t) = show t ++ "[]"
  show BooleanType = "bool"
  show ByteType = "byte"
  show CharType = "char"
  show DoubleType = "double"
  show FloatType = "float"
  show IntType = "int"
  show LongType = "long"
  show NullType = "null"
  show (RefType s) = show s
  show ShortType = "short"
  show UnknownType = "<untyped>"
  show VoidType = "void"

instance Hashable Type where
  hashWithSalt n t = n + hash (show t)

instance Arbitrary Type where
  arbitrary = oneof $ refType:(map return [
    BooleanType,
    ByteType,
    CharType,
    DoubleType,
    FloatType,
    IntType,
    LongType,
    NullType,
    ShortType])
    where
      refType = fmap RefType arbitrary

isNonvoidType :: Type -> Bool
isNonvoidType VoidType = False
isNonvoidType UnknownType = False
isNonvoidType _ = True

isBaseType :: Type -> Bool
isBaseType VoidType = False
isBaseType UnknownType = False
isBaseType (ArrayType _) = False
isBaseType _ = True

isIntegerType :: Type -> Bool
isIntegerType BooleanType = True
isIntegerType ByteType = True
isIntegerType CharType = True
isIntegerType IntType = True
isIntegerType LongType = True
isIntegerType ShortType = True
isIntegerType _ = False

data MethodBody
  = EmptyBody
  | FullBody { declarations :: [Declaration]
             , statements :: [Statement]
             , catchClauses :: [CatchClause]
             } deriving (Show, Eq)

type Declaration = (Type, [String])

data Statement
  = Label String
  | Breakpoint
  -- | Entermonitor Immediate -- Don't use this!
  -- | Exitmonitor Immediate -- Don't use this!
  | Tableswitch Immediate [CaseStatement]
  | Lookupswitch Immediate [CaseStatement]
  | Identity String AtIdentifier Type
  | IdentityNoType String AtIdentifier
  | Assign Variable Expr
  | If BoolExpr String
  | Goto String
  | Nop
  | Ret (Maybe Immediate)
  | Return (Maybe Immediate)
  | Throw Immediate
  | Invoke EInvoke deriving (Show, Eq)

type CaseStatement = (CaseLabel, String)

data CaseLabel
  = ConstantCase Int
  | DefaultCase deriving (Show, Eq)

data CatchClause = CatchClause { className :: String
                               , fromLabel :: String -- First label in try block
                               , toLabel   :: String -- Last label in catch block continue parsing until next label
                               , withLabel :: String -- Execute code below this label
                               } deriving (Show, Eq)

data Expr
  = NewExpr Type
  | NewArrayExpr Type Immediate
  | NewMultiArrayExpr Type [Immediate]
  | CastExpr Type Immediate
  | InstanceOfExpr Immediate Type
  | InvokeExpr EInvoke
  | RefExpr ERef
  | BinopExpr Immediate Binop Immediate
  | UnopExpr Unop Immediate
  | ImmediateExpr Immediate
  | MethodHandle MethodSignature deriving (Show, Eq)

instance Arbitrary Expr where
  arbitrary = oneof [
    fmap NewExpr arbitrary,
    liftM2 NewArrayExpr arbitrary arbitrary,
    liftM2 NewMultiArrayExpr arbitrary (replicateM 2 arbitrary),
    liftM2 CastExpr arbitrary arbitrary,
    liftM2 InstanceOfExpr arbitrary arbitrary,
    -- InvokeExpr EInvoke,
    -- RefExpr ERef,
    liftM3 BinopExpr arbitrary arbitrary arbitrary,
    liftM2 UnopExpr arbitrary arbitrary,
    fmap ImmediateExpr arbitrary]
    -- MethodHandle MethodSignature]

data BoolExpr
  = BoolExpr Immediate BoolOp Immediate deriving (Show, Eq)

instance Arbitrary BoolExpr where
  arbitrary = liftM3 BoolExpr arbitrary arbitrary arbitrary

data Immediate
  = Local String
  | DoubleConstant Float
  | FloatConstant Float
  | IntConstant Int
  | LongConstant Int
  | NullConstant
  | StringConstant String
  | ClassConstant String deriving (Show, Eq)

instance Arbitrary Immediate where
  arbitrary = oneof [
    fmap Local arbitrary,
    fmap DoubleConstant arbitrary,
    fmap FloatConstant arbitrary,
    fmap IntConstant arbitrary,
    fmap LongConstant arbitrary,
    return NullConstant,
    fmap StringConstant arbitrary,
    fmap ClassConstant arbitrary]

data AtIdentifier
  = ThisRef
  | ParameterRef Int
  | CaughtExceptionRef deriving (Show, Eq)

data Variable
  = ReferenceVar ERef
  | LocalVar String deriving (Show, Eq)

data ERef
  = ArrayRef String Immediate
  | FieldRef String FieldSignature
  | SignatureRef FieldSignature deriving (Show, Eq)

data EInvoke
  = SpecialInvoke String MethodSignature [Immediate]
  | VirtualInvoke String MethodSignature [Immediate]
  | InterfaceInvoke String MethodSignature [Immediate]
  | StaticInvoke MethodSignature [Immediate]
  | DynamicInvoke String UnnamedMethodSignature [Immediate] MethodSignature [Immediate] deriving (Show, Eq)

data BoolOp
  = Cmpeq | Cmpne | Cmpgt | Cmpge | Cmplt | Cmple deriving (Show, Eq)

instance Arbitrary BoolOp where
  arbitrary = oneof $ map return [Cmpeq, Cmpne, Cmpgt, Cmpge, Cmplt, Cmple]

data Binop
  = And | Or | Xor | Shl | Shr | Ushr
  | Cmp | Cmpg | Cmpl
  | Plus | Minus | Mult | Div | Rem deriving (Show, Eq)

instance Arbitrary Binop where
  arbitrary = oneof $ map return [And, Or, Xor, Shl, Shr, Ushr,
                                  Cmp, Cmpg, Cmpl,
                                  Plus, Minus, Mult, Div, Rem]

data Unop
  = Lengthof
  | Neg deriving (Show, Eq)

instance Arbitrary Unop where
  arbitrary = oneof $ map return [Lengthof, Neg]

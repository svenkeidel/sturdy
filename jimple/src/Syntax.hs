{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet (HashSet)
import           Data.Identifiable

import           GHC.Generics (Generic)
import           Text.Printf

---- Data ----------------------------------------------------------------------

type Label = Int

type ClassTable = ClassTable' Label
newtype ClassTable' label = ClassTable (HashMap ClassId (CompilationUnit' label))

type CompilationUnit = CompilationUnit' Label
data CompilationUnit' label
  = Class
  { modifiers :: HashSet Modifier
  , classExtends :: Maybe ClassId
  , implements :: Vector ClassId
  , classFields :: HashMap FieldName Field
  , methods :: HashMap MethodSignature (Method' label)
  }
  | Interface
  { modifiers :: HashSet Modifier
  , interfaceExtends :: Vector ClassId
  , methods :: HashMap MethodSignature (Method' label)
  }

type ClassName = Text
type MethodName = Text
type FieldName = Text

data ClassId
  = ClassId
  { classHash :: Int
  , classPackage :: Vector Text
  , className :: Text
  }
  deriving stock (Eq)

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
  | Annotation
  deriving stock (Eq)

data Type
  = Array Type
  | Boolean
  | Byte
  | Char
  | Double
  | Float
  | Int
  | Long
  | Object ClassId
  | Short
  | Unknown
  | Void
  deriving stock (Eq,Generic)
  deriving anyclass (Hashable)

data Field = Field
  { fieldModifiers :: HashSet Modifier
  , fieldType :: Type
  }

data MethodSignature
  = MethodSignature
  { methodHash :: Int
  , methodReturnType :: Type
  , methodName :: MethodName
  , methodArgumentTypes :: Vector Type
  } deriving (Eq)

type Method = Method' Label
data Method' label = Method
  { methodModifiers :: HashSet Modifier
  , throws :: HashSet ClassId
  , methodBody :: Maybe (MethodBody' label)
  } deriving (Show, Eq)

type MethodBody = MethodBody' Label
data MethodBody' label
  = MethodBody
  { declarations :: HashMap Text Type
  , statements :: Vector (Statement' label)
  , catchClauses :: HashMap ClassId (CatchClause' label)
  }
  deriving (Show, Eq)

type Statement = Statement' Label
data Statement' label
  = Breakpoint
  | EnterMonitor Immediate
  | ExitMonitor Immediate
  | TableSwitch Immediate Int (Vector label) label
  | LookupSwitch Immediate (IntMap label) label
  | Identity Text Variable (Maybe Type)
  | Assign Variable Expr
  | If Expr label
  | Goto label
  | Label label
  | Nop
  | Ret (Maybe Immediate)
  | Return (Maybe Immediate)
  | Throw Immediate
  | InvokeStmt Invoke
  deriving (Show, Eq)

type CatchClause = CatchClause' Label
data CatchClause' label
  = CatchClause
  { fromLabel     :: label -- First label in try block label
  , toLabel       :: label -- Last label in catch block continue parsing until next label
  , withLabel     :: label -- Execute code below this label
  }
  deriving (Show, Eq)

data Expr
  = New Type
  | NewArray Type Immediate
  | NewMultiArray Type [Immediate]
  | Cast Type Immediate
  | InstanceOf Immediate Type
  | InvokeExpr Invoke
  | Ref Reference
  | Binop Immediate Binop Immediate
  | Unop Unop Immediate
  | Immediate Immediate
  | MethodHandle MethodSignature
  deriving (Show, Eq)

data Immediate
  = Local Text
  | DoubleConstant Double
  | FloatConstant Float
  | IntConstant Int32
  | LongConstant Int64
  | NullConstant
  | StringConstant Text
  | ClassConstant Text
  deriving (Show, Eq)

data Variable
  = ReferenceVar Reference
  | LocalVar Text
  | This
  | Parameter Int
  | CaughtException
  | StaticInstance ClassId
  deriving (Show, Eq)

data Reference
  = ArrayRef Text Immediate
  | FieldRef Text Type
  | SignatureRef Type
  deriving (Show, Eq)

data Invoke
  = InvokeSpecial   Variable ClassId MethodSignature [Immediate]
  | InvokeVirtual   Variable ClassId MethodSignature [Immediate]
  | InvokeInterface Variable ClassId MethodSignature [Immediate]
  | InvokeStatic             ClassId MethodSignature [Immediate]
  -- | InvokeDynamic -- Currently not supported
  deriving (Show, Eq)

data Binop
  = And | Or | Xor | Mod | Rem
  | Cmp | Cmpg | Cmpl
  | Shl | Shr | Ushr
  | Plus | Minus | Mult | Div
  deriving (Show, Eq)

data Unop
  = Lengthof
  | Neg
  deriving (Show, Eq)

---- Resolving Labels -----------------------------------------------------------------

class ResolveLabels stmts where
  resolveLabels :: (Identifiable label,Show label) => stmts label -> stmts Label

instance ResolveLabels ClassTable' where
  resolveLabels (ClassTable t) = ClassTable (Map.map resolveLabels t)

instance ResolveLabels CompilationUnit' where
  resolveLabels Class{..} = Class { modifiers = modifiers, classExtends = classExtends, implements = implements, classFields = classFields, methods = Map.map resolveLabels methods }
  resolveLabels Interface{..} = Interface { modifiers = modifiers, interfaceExtends = interfaceExtends, methods = Map.map resolveLabels methods }

instance ResolveLabels Method' where
  resolveLabels Method{..} = Method { methodModifiers = methodModifiers, throws = throws, methodBody = resolveLabels <$> methodBody }

instance ResolveLabels MethodBody' where
  resolveLabels MethodBody{..}
    = MethodBody
    { declarations = declarations
    , statements = Vec.map (resolveLabelsWith resolve) statements
    , catchClauses = Map.map (resolveLabelsWith resolve) catchClauses
    }
    where
      labs = labels statements
      resolve lab = case Map.lookup lab labs of
        Just i -> i
        Nothing -> error (printf "Label %s not found" (show lab))

      labels :: Identifiable label => Vector (Statement' label) -> HashMap label Int
      labels = Vec.ifoldl (\m i s -> case s of Label l -> Map.insert l i m; _ -> m) Map.empty

class ResolveLabelsWith stmts where
  resolveLabelsWith :: Identifiable label => (label -> Label) -> stmts label -> stmts Label

instance ResolveLabelsWith Statement' where
  resolveLabelsWith resolve stmt = case stmt of
    TableSwitch key offset cases def -> TableSwitch key offset (Vec.map resolve cases) (resolve def)
    LookupSwitch key cases def -> LookupSwitch key (IntMap.map resolve cases) (resolve def)
    If expr label -> If expr (resolve label)
    Goto label -> Goto (resolve label)
    Label label -> Label (resolve label)
    Breakpoint -> Breakpoint
    EnterMonitor x -> EnterMonitor x
    ExitMonitor x -> ExitMonitor x
    Identity to from typ -> Identity to from typ
    Assign to expr -> Assign to expr
    Nop -> Nop
    Ret ret -> Ret ret
    Return ret -> Return ret
    Throw x -> Throw x
    InvokeStmt m -> InvokeStmt m

instance ResolveLabelsWith CatchClause' where
  resolveLabelsWith resolve CatchClause{..}
    = CatchClause
    { fromLabel = resolve fromLabel
    , toLabel = resolve toLabel
    , withLabel = resolve withLabel
    }


---- Helpers -------------------------------------------------------------------

classId :: [Text] -> Text -> ClassId
classId pkg name = ClassId { classHash = hash (pkg,name), classPackage = Vec.fromList pkg, className = name }

methodSignature :: Type -> MethodName -> [Type] -> MethodSignature
methodSignature retType name argTypes
  = MethodSignature
  { methodReturnType = retType
  , methodName = name
  , methodArgumentTypes = Vec.fromList argTypes
  , methodHash = hash (retType,name,argTypes)
  }

isNonvoidType :: Type -> Bool
isNonvoidType Void = False
isNonvoidType Unknown = False
isNonvoidType _ = True

isBaseType :: Type -> Bool
isBaseType Void = False
isBaseType Unknown = False
isBaseType (Array _) = False
isBaseType _ = True

isIntegerType :: Type -> Bool
isIntegerType Boolean = True
isIntegerType Byte = True
isIntegerType Char = True
isIntegerType Int = True
isIntegerType Long = True
isIntegerType Short = True
isIntegerType _ = False


---- Instances -----------------------------------------------------------------

instance Show ClassId where
  show c = Text.unpack $ Text.intercalate "." (Vec.toList (classPackage c)) `Text.append` "." `Text.append` className c

instance Hashable ClassId where
  hashWithSalt s c = s `hashWithSalt` classHash c
  hash = classHash

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

instance Hashable MethodSignature where
  hashWithSalt s sig = s `hashWithSalt` methodHash sig
  hash = methodHash

instance Show MethodSignature where
  show m = printf "%s %s%s"
    (show (methodReturnType m))
    (Text.unpack (methodName m))
    (show (Vec.toList (methodArgumentTypes m)))

-- instance Ord FieldSignature where
--   compare (FieldSignature c1 _ n1) (FieldSignature c2 _ n2) =
--     case compare c1 c2 of
--       LT -> LT
--       EQ -> compare n1 n2
--       GT -> GT

-- instance Show FieldSignature where
--   show (FieldSignature c t n) =
--     "<" ++ show c ++ ": " ++ show t ++ " " ++ show n ++ ">"

-- instance Hashable FieldSignature where
--   hashWithSalt s (FieldSignature c t n) = s + hash c + hash t + hash n

instance Show Type where
  show (Array t) = show t ++ "[]"
  show Boolean = "bool"
  show Byte = "byte"
  show Char = "char"
  show Double = "double"
  show Float = "float"
  show Int = "int"
  show Long = "long"
  show (Object s) = show s
  show Short = "short"
  show Unknown = "<untyped>"
  show Void = "void"

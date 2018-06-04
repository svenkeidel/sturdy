{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

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

data Member = FieldMember Field | MethodMember Method

data Field = Field { fieldModifiers :: [Modifier]
                   , fieldType :: Type
                   , fieldName :: String
                   }

data Method = Method { methodModifiers :: [Modifier]
                     , returnType :: Type
                     , methodName :: String
                     , parameters :: [Type]
                     , throws :: [String]
                     , methodBody :: MethodBody
                     } deriving (Show, Eq)

data Type
  = ArrayType Type
  | BooleanType
  | BottomType
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
  show BottomType = "⊥"
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

isIntegerType :: Type -> Bool
isIntegerType BooleanType = True
isIntegerType ByteType = True
isIntegerType CharType = True
isIntegerType IntType = True
isIntegerType ShortType = True
isIntegerType _ = False

isNonvoidType :: Type -> Bool
isNonvoidType VoidType = False
isNonvoidType UnknownType = False
isNonvoidType _ = True

isBaseType :: Type -> Bool
isBaseType VoidType = False
isBaseType UnknownType = False
isBaseType (ArrayType _) = False
isBaseType _ = True

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
  -- | Entermonitor Expr -- Don't use this!
  -- | Exitmonitor Expr -- Don't use this!
  | Tableswitch Expr [CaseStatement]
  | Lookupswitch Expr [CaseStatement]
  | Identity String Expr Type
  | IdentityNoType String Expr
  | Assign Variable Expr
  | If Expr String
  | Goto String
  | Nop
  | Ret (Maybe Expr)
  | Return (Maybe Expr)
  | Throw Expr
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
  | NewArrayExpr Type Expr
  | NewMultiArrayExpr Type [Expr]
  | CastExpr Type Expr
  | InstanceOfExpr Expr Type
  | InvokeExpr EInvoke
  | ThisRef
  | ParameterRef Int
  | CaughtExceptionRef
  | ArrayRef String Expr
  | FieldRef String FieldSignature
  | SignatureRef FieldSignature
  | BinopExpr Expr Binop Expr
  | UnopExpr Unop Expr
  | Local String
  | DoubleConstant Float
  | FloatConstant Float
  | IntConstant Int
  | LongConstant Int
  | NullConstant
  | StringConstant String
  | ClassConstant String
  | MethodHandle MethodSignature deriving (Show, Eq)

data Variable
  = ReferenceVar Expr
  | LocalVar String deriving (Show, Eq)

data EInvoke
  = SpecialInvoke String MethodSignature [Expr]
  | VirtualInvoke String MethodSignature [Expr]
  | InterfaceInvoke String MethodSignature [Expr]
  | StaticInvoke MethodSignature [Expr]
  | DynamicInvoke String UnnamedMethodSignature [Expr] MethodSignature [Expr] deriving (Show, Eq)

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

data Binop
  = And | Or | Xor | Shl | Shr | Ushr
  | Cmp | Cmpg | Cmpl
  | Cmpeq | Cmpne | Cmpgt | Cmpge | Cmplt | Cmple
  | Plus | Minus | Mult | Div | Rem deriving (Show, Eq)

data Unop
  = Lengthof
  | Neg deriving (Show, Eq)

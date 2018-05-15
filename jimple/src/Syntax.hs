{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax where

data AtIdentifier
  = IDParameter Int
  | IDThis
  | IDCaughtException deriving (Eq)

instance Show AtIdentifier where
  show (IDParameter n) = "@parameter" ++ show n
  show IDThis = "@this"
  show IDCaughtException = "@caughtexception"

data File = File { fileModifiers :: [Modifier]
                 , fileType :: FileType
                 , fileName :: String
                 , extends :: Maybe String
                 , implements :: [String]
                 , fileBody :: [Member]
                 }

instance Eq File where
  (==) f1 f2 = fileName f1 == fileName f2

instance Show File where
  show f = "File {"
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
  = FTClass
  | FTInterface

instance Show FileType where
  show FTClass = "class"
  show FTInterface = "interface"

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
  = TUnknown
  | TVoid
  | TBoolean
  | TByte
  | TChar
  | TShort
  | TInt
  | TLong
  | TFloat
  | TDouble
  | TNull
  | TClass String
  | TArray Type deriving (Show, Eq)

isNonvoidType :: Type -> Bool
isNonvoidType TVoid = False
isNonvoidType TUnknown = False
isNonvoidType _ = True

isBaseType :: Type -> Bool
isBaseType TVoid = False
isBaseType TUnknown = False
isBaseType (TArray _) = False
isBaseType _ = True

data MethodBody
  = MEmpty
  | MFull { declarations :: [Declaration]
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
  | If Expr String
  | Goto String
  | Nop
  | Ret (Maybe Immediate)
  | Return (Maybe Immediate)
  | Throw Immediate
  | Invoke InvokeExpr deriving (Show, Eq)

type CaseStatement = (CaseLabel, String)

data CaseLabel
  = CLConstant Int
  | CLDefault deriving (Show, Eq)

data CatchClause = CatchClause { className :: String
                               , fromLabel :: String -- First label in try block
                               , toLabel   :: String -- Last label in catch block continue parsing until next label
                               , withLabel :: String -- Execute code below this label
                               } deriving (Show, Eq)

data Expr
  = ENew NewExpr
  | ECast Type Immediate
  | EInstanceof Immediate Type
  | EInvoke InvokeExpr
  | EReference Reference
  | EBinop Immediate Binop Immediate
  | EUnop Unop Immediate
  | EImmediate Immediate deriving (Show, Eq)

data NewExpr
  = NewSimple Type
  | NewArray Type Immediate
  | NewMulti Type [Immediate] deriving (Show, Eq)

data Variable
  = VReference Reference
  | VLocal String deriving (Show, Eq)

data InvokeExpr
  = SpecialInvoke String MethodSignature [Immediate]
  | VirtualInvoke String MethodSignature [Immediate]
  | InterfaceInvoke String MethodSignature [Immediate]
  | StaticInvoke MethodSignature [Immediate]
  | DynamicInvoke String UnnamedMethodSignature [Immediate] MethodSignature [Immediate] deriving (Show, Eq)

data MethodSignature = MethodSignature String Type String [Type] deriving (Show, Eq)

data UnnamedMethodSignature = UnnamedMethodSignature Type [Type] deriving (Show, Eq)

data Reference
  = ArrayReference String Immediate
  | FieldReference String FieldSignature
  | SignatureReference FieldSignature deriving (Show, Eq)

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

data Immediate
  = ILocalName String
  | IInt Int
  | IFloat Float
  | IString String
  | IClass String
  | INull deriving (Show, Eq)

data Binop
  = And     -- Bytewise operator
  | Or      -- Bytewise operator
  | Xor     -- Bytewise operator
  | Mod
  | Rem
  | Cmp
  | Cmpg
  | Cmpl
  | Cmpeq
  | Cmpne
  | Cmpgt
  | Cmpge
  | Cmplt
  | Cmple
  | Shl     -- Bytewise operator
  | Shr     -- Bytewise operator
  | Ushr    -- Bytewise operator
  | Plus
  | Minus
  | Mult
  | Div deriving (Show, Eq)

data Unop
  = Lengthof
  | Neg deriving (Show, Eq)

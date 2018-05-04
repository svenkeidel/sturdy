{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

data AtIdentifier
  = IDParameter Int
  | IDThis
  | IDCoughtException deriving (Eq)

data File = File { modifiers :: [Modifier]
                 , fileType :: FileType
                 , className :: String
                 , extends :: String
                 , implements :: [String]
                 , body :: [Member]
                 }

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

data FileType
  = FTClass
  | FTInterface

data Member
  = Field { modifiers :: [Modifier]
          , fieldType :: Type
          , name :: String
          }
  | Method { modifiers :: [Modifier]
           , returnType :: Type
           , name :: String
           , parameters :: [Type]
           , throws :: [String]
           , body :: MethodBody
           }

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
  | TArray Type deriving (Eq, Show)

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
          }

type Declaration = (Type, [String])

data Statement
  = Label String
  | Breakpoint
  | Entermonitor Immediate
  | Exitmonitor Immediate
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
  | Invoke InvokeExpr deriving (Eq)

type CaseStatement = (CaseLabel, String)

data CaseLabel
  = CLConstant Int
  | CLDefault deriving (Eq)

data CatchClause = CatchClause { className :: String
                               , fromLabel :: String -- First label in try block
                               , toLabel   :: String -- Last label in catch block continue parsing until next label
                               , withLabel :: String -- Execute code below this label
                               }

data Expr
  = ENew NewExpr
  | ECast Type Immediate
  | EInstanceof Immediate Type
  | EInvoke InvokeExpr
  | EReference Reference
  | EBinop Immediate Binop Immediate
  | EUnop Unop Immediate
  | EImmediate Immediate deriving (Eq)

data NewExpr
  = NewSimple Type
  | NewArray Type Immediate
  | NewMulti Type [Immediate] deriving (Eq)

data Variable
  = VReference Reference
  | VLocal String deriving (Eq)

data InvokeExpr
  = SpecialInvoke String MethodSignature [Immediate]
  | VirtualInvoke String MethodSignature [Immediate]
  | InterfaceInvoke String MethodSignature [Immediate]
  | StaticInvoke MethodSignature [Immediate]
  | DynamicInvoke String UnnamedMethodSignature [Immediate] MethodSignature [Immediate] deriving (Eq)

data UnnamedMethodSignature = UnnamedMethodSignature { returnType :: Type
                                                     , parameters :: [Type]
                                                     } deriving (Eq)

data MethodSignature = MethodSignature { className :: String
                                       , returnType :: Type
                                       , methodName :: String
                                       , parameters :: [Type]
                                       } deriving (Eq)

data Reference
  = ArrayReference String Immediate
  | FieldReference String FieldSignature
  | SignatureReference FieldSignature deriving (Eq)

data FieldSignature = FieldSignature { className :: String
                                     , fieldType :: Type
                                     , fieldName :: String
                                     } deriving (Eq)

data Immediate
  = ILocalName String
  | IInt Int
  | IFloat Float
  | IString String
  | IClass String
  | INull deriving (Eq)

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
  | Div deriving (Eq)

data Unop
  = Lengthof
  | Neg deriving (Eq)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax where

import GHC.Generics (Generic)
import Data.Hashable

data AtIdentifier
  = IDParameter Int
  | IDThis
  | IDCoughtException deriving (Eq)

data File = File { fileModifiers :: [Modifier]
                 , fileType :: FileType
                 , fileName :: String
                 , extends :: Maybe String
                 , implements :: [String]
                 , body :: [Member]
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
  | Annotation

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

instance Hashable Type where
  hashWithSalt s TUnknown = s `hashWithSalt` "Unknown"
  hashWithSalt s TVoid = s `hashWithSalt` "Void"
  hashWithSalt s TBoolean = s `hashWithSalt` "Boolean"
  hashWithSalt s TByte = s `hashWithSalt` "Byte"
  hashWithSalt s TChar = s `hashWithSalt` "Char"
  hashWithSalt s TShort = s `hashWithSalt` "Short"
  hashWithSalt s TInt = s `hashWithSalt` "Int"
  hashWithSalt s TLong = s `hashWithSalt` "Long"
  hashWithSalt s TFloat = s `hashWithSalt` "Float"
  hashWithSalt s TDouble = s `hashWithSalt` "Double"
  hashWithSalt s TNull = s `hashWithSalt` "Null"
  hashWithSalt s (TClass c) = s `hashWithSalt` "Class" `hashWithSalt` c
  hashWithSalt s (TArray t) = s `hashWithSalt` "Array" `hashWithSalt` t

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

data UnnamedMethodSignature = UnnamedMethodSignature Type [Type] deriving (Eq)

data MethodSignature = MethodSignature String Type String [Type] deriving (Eq)

data Reference
  = ArrayReference String Immediate
  | FieldReference String FieldSignature
  | SignatureReference FieldSignature deriving (Eq)

data FieldSignature = FieldSignature String Type String deriving (Generic, Eq)

instance Hashable FieldSignature where
  hashWithSalt s (FieldSignature c t n) =
    s `hashWithSalt` c `hashWithSalt` t `hashWithSalt` n

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

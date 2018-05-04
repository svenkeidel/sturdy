{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

-- Helpers
--   all = [0 .. 0xffff];
--
--   dec_digit = ['0' .. '9'];
--   dec_nonzero = ['1' .. '9'];
--   dec_constant = dec_digit+;
--
--   hex_digit = dec_digit | ['a' .. 'f'] | ['A' .. 'F'];
--   hex_constant = '0' ('x' | 'X') hex_digit+;
--
--   oct_digit = ['0' .. '7'];
--   oct_constant = '0' oct_digit+;
--
--   quote = ''';
--
--   escapable_char = '\' | ' ' | quote | '.' | '#' | '"' | 'n' | 't' | 'r' | 'b' | 'f';
--   escape_code = 'u' hex_digit hex_digit hex_digit hex_digit;
--   escape_char = '\' (escapable_char | escape_code);
--
--   not_cr_lf = [all - [10 + 13]];
--   not_star = [all - '*'];
--   not_star_slash = [not_star - '/'];
--
--   alpha_char = ['a' .. 'z'] | ['A' .. 'Z'];
--
--   simple_id_char = alpha_char | dec_digit | '_' | '$' | '-';
--
--   first_id_char = alpha_char | '_' | '$';
--
--   quotable_char = [not_cr_lf - '''];
--
--   // string_char = [not_cr_lf - '"'];
--
--   // escapes and any char except '\' (92) or '"' (34).
--   string_char = escape_char | [0 .. 33] | [35 .. 91] | [93 ..127] ;
--
--   line_comment = '//' not_cr_lf*;
--   long_comment = '/*' not_star* '*'+ (not_star_slash not_star* '*'+)* '/';
--
--   blank = (' ' | 9 | 13 | 10)+;
--   ignored_helper = (blank | line_comment | long_comment)+;
--
-- Tokens
--   ignored = ignored_helper;
--   abstract = 'abstract';
--   final = 'final';
--   native = 'native';
--   public = 'public';
--   protected = 'protected';
--   private = 'private';
--   static = 'static';
--   synchronized = 'synchronized';
--   transient = 'transient';
--   volatile = 'volatile';
--   strictfp = 'strictfp';
--   enum = 'enum';
--   annotation = 'annotation';
--
--   class = 'class';
--   interface = 'interface';
--
--   void = 'void';
--   boolean = 'boolean';
--   byte = 'byte';
--   short = 'short';
--   char = 'char';
--   int = 'int';
--   long = 'long';
--   float = 'float';
--   double = 'double';
--   null_type = 'null_type';
--   unknown = 'unknown';
--
--   extends = 'extends';
--   implements = 'implements';
--
--   breakpoint = 'breakpoint';
--   case = 'case';
--   catch = 'catch';
--   cmp = 'cmp';
--   cmpg = 'cmpg';
--   cmpl = 'cmpl';
--   default = 'default';
--   entermonitor = 'entermonitor';
--   exitmonitor = 'exitmonitor';
--   goto = 'goto';
--   if = 'if';
--   instanceof = 'instanceof';
--   interfaceinvoke = 'interfaceinvoke';
--   lengthof = 'lengthof';
--   lookupswitch = 'lookupswitch';
--   neg = 'neg';
--   new = 'new';
--   newarray = 'newarray';
--   newmultiarray = 'newmultiarray';
--   nop = 'nop';
--   ret = 'ret';
--   return = 'return';
--   specialinvoke = 'specialinvoke';
--   staticinvoke = 'staticinvoke';
--   dynamicinvoke = 'dynamicinvoke';
--   tableswitch = 'tableswitch';
--   throw = 'throw';
--   throws = 'throws';
--   virtualinvoke = 'virtualinvoke';
--   null = 'null';
--   from = 'from';
--   to = 'to';
--   with = 'with';
--   cls = 'cls';
--
--   comma = ',';
--   l_brace = '{';
--   r_brace = '}';
--   semicolon = ';';
--   l_bracket = '[';
--   r_bracket = ']';
--   l_paren = '(';
--   r_paren = ')';
--   colon = ':';
--   dot = '.';
--   quote = ''';
--   colon_equals = ':=';
--   equals = '=';
--   and = '&';
--   or = '|';
--   xor = '^';
--   mod = '%';
--   cmpeq = '==';
--   cmpne = '!=';
--   cmpgt = '>';
--   cmpge = '>=';
--   cmplt = '<';
--   cmple = '<=';
--   shl = '<<';
--   shr = '>>';
--   ushr = '>>>';
--   plus = '+';
--   minus = '-';
--   mult = '*';
--   div = '/';
--
--   quoted_name = quote quotable_char+ quote;
--
--   full_identifier =
--       ((first_id_char | escape_char | quote) (simple_id_char | escape_char)* (quote)? '.')+
--       (first_id_char | escape_char | quote) (simple_id_char | escape_char)* (quote)?;
--   identifier =
--       (first_id_char | escape_char) (simple_id_char | escape_char)* | '<clinit>' | '<init>';
type Identifier = String
--
--   at_identifier = '@' (('parameter' dec_digit+ ':') | 'this' ':' | 'caughtexception');
data AtIdentifier
  = IDParameter Int
  | IDThis
  | IDCoughtException deriving (Eq)
--
--   bool_constant = 'true' | 'false';
--   integer_constant = (dec_constant | hex_constant | oct_constant) 'L'?;
--   float_constant = ((dec_constant '.' dec_constant) (('e'|'E') ('+'|'-')? dec_constant)? ('f'|'F')?)  | ('#' (('-'? 'Infinity') | 'NaN') ('f' | 'F')? ) ;
--   string_constant = '"' string_char* '"';
--
-- Ignored Tokens
--   ignored;
--
-- Productions
--   file =
--     modifier* file_type class_name extends_clause? implements_clause? file_body;
data File = File { modifiers :: [Modifier]
                 , fileType :: FileType
                 , className :: ClassName
                 , extends :: Maybe ExtendsClause
                 , implements :: Maybe ImplementsClause
                 , body :: FileBody
                 }
--
--   modifier =
--     {abstract}     abstract |
--     {final}        final |
--     {native}       native |
--     {public}       public |
--     {protected}    protected |
--     {private}      private |
--     {static}       static |
--     {synchronized} synchronized |
--     {transient}    transient |
--     {volatile}     volatile |
--     {strictfp}     strictfp |
--     {enum}         enum |
--     {annotation}   annotation;
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

--   file_type =
--     {class}     [theclass]:class |
--     {interface} interface;
data FileType
  = FTClass
  | FTInterface
--
--   extends_clause =
--     extends class_name;
type ExtendsClause = ClassName
--
--   implements_clause =
--     implements class_name_list;
type ImplementsClause = ClassNameList
--
--   file_body =
--     l_brace member* r_brace;
type FileBody = [Member]
--
--   name_list =
--     {single} name |
--     {multi}  name comma name_list;
type NameList = [Name]
--
-- class_name_list =
--     {class_name_single} class_name |
--     {class_name_multi}  class_name comma class_name_list;
type ClassNameList = [ClassName]
--
--   member =
--     {field}  modifier* type name semicolon |
--     {method} modifier* type name l_paren parameter_list? r_paren throws_clause? method_body;
data Member
  = Field { modifiers :: [Modifier]
          , fieldType :: Type
          , name :: Name
          }
  | Method { modifiers :: [Modifier]
           , returnType :: Type
           , name :: Name
           , parameters :: Maybe ParameterList
           , throws :: Maybe ThrowsClause
           , body :: MethodBody
           }
--   type =
--     {void}   void |
--     {nonvoid} nonvoid_type;
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
--
--   parameter_list =
--     {single} parameter |
--     {multi}  parameter comma parameter_list;
type ParameterList = [Parameter]
--
--   parameter =
--     nonvoid_type;
type Parameter = Type
--
--   throws_clause =
--     throws class_name_list;
type ThrowsClause = ClassNameList
--
--   base_type_no_name =
--     {boolean} boolean |
--     {byte}    byte |
--     {char}    char |
--     {short}   short |
--     {int}     int |
--     {long}    long |
--     {float}   float |
--     {double}  double |
--     {null}    null_type;
--
--   base_type =
--     {boolean} boolean |
--     {byte}    byte |
--     {char}    char |
--     {short}   short |
--     {int}     int |
--     {long}    long |
--     {float}   float |
--     {double}  double |
--     {null}    null_type |
--     {class_name}    class_name;
--
--   nonvoid_type =
--     {base}   base_type_no_name array_brackets* |
--     {quoted} quoted_name array_brackets* |
--     {ident}  identifier array_brackets* |
--     {full_ident} full_identifier array_brackets*;
--
--   array_brackets =
--     l_bracket r_bracket;
--
--   method_body =
--     {empty} semicolon |
--     {full}  l_brace declaration* statement* catch_clause* r_brace;
data MethodBody
  = MEmpty
  | MFull { declarations :: [Declaration]
          , statements :: [Statement]
          , catchClauses :: [CatchClause]
          }
--   declaration =
--     jimple_type local_name_list semicolon;
type Declaration = (Type, LocalNameList)
--
--   jimple_type =
--     {unknown} unknown |
--     {nonvoid} nonvoid_type;
--
--   local_name =
--     name;
type LocalName = Name
--
--   local_name_list =
--     {single} local_name |
--     {multi}  local_name comma local_name_list;
type LocalNameList = [LocalName]
--
--   statement =
--     {label}        label_name colon |
--     {breakpoint}   breakpoint semicolon |
--     {entermonitor} entermonitor immediate semicolon |
--     {exitmonitor}  exitmonitor immediate semicolon |
--     {tableswitch}  tableswitch l_paren immediate r_paren l_brace case_stmt+ r_brace semicolon |
--     {lookupswitch} lookupswitch l_paren immediate r_paren l_brace case_stmt+ r_brace semicolon |
--     {identity}     local_name colon_equals at_identifier type semicolon |
--     {identity_no_type}  local_name colon_equals at_identifier semicolon |
--     {assign}       variable equals expression semicolon |
--     {if}           if bool_expr goto_stmt |
--     {goto}         goto_stmt |
--     {nop}          nop semicolon |
--     {ret}          ret immediate? semicolon |
--     {return}       return immediate? semicolon |
--     {throw}        throw immediate semicolon |
--     {invoke}       invoke_expr semicolon;
data Statement
  = Label LabelName
  | Breakpoint
  | Entermonitor Immediate
  | Exitmonitor Immediate
  | Tableswitch Immediate [CaseStatement]
  | Lookupswitch Immediate [CaseStatement]
  | Identity LocalName AtIdentifier Type
  | IdentityNoType LocalName AtIdentifier
  | Assign Variable Expr
  | If Expr GotoStatement
  | Goto GotoStatement
  | Nop
  | Ret (Maybe Immediate)
  | Return (Maybe Immediate)
  | Throw Immediate
  | Invoke Expr deriving (Eq)

--
--   label_name =
--     identifier;
type LabelName = Identifier
--
--   case_stmt =
--     case_label colon goto_stmt;
type CaseStatement = (CaseLabel, GotoStatement)
--
--   case_label =
--     {constant} case minus? integer_constant |
--     {default}  default;
data CaseLabel
  = CLConstant Int
  | CLDefault deriving (Eq)
--
--   goto_stmt =
--     goto label_name semicolon;
type GotoStatement = LabelName
--
--   catch_clause =
--     catch [name]:class_name from [from_label]:label_name to [to_label]:label_name with [with_label]:label_name semicolon;
data CatchClause = CatchClause { className :: ClassName
                               , fromLabel :: LabelName -- First label in try block
                               , toLabel   :: LabelName -- Last label in catch block continue parsing until next label
                               , withLabel :: LabelName -- Execute code below this label
                               }
--
--   expression =
--     {new}         new_expr |
--     {cast}        l_paren nonvoid_type r_paren immediate |
--     {instanceof}  immediate instanceof nonvoid_type |
--     {invoke}      invoke_expr |
--     {reference}   reference |
--     {binop}       binop_expr |
--     {unop}        unop_expr |
--     {immediate}   immediate;
data Expr
  = ENew NewExpr
  | ECast Type Immediate
  | EInstanceof Immediate Type
  | EInvoke InvokeExpr
  | EReference Reference
  | EBinop Immediate Binop Immediate
  | EUnop Unop Immediate
  | EImmediate Immediate deriving (Eq)
--
--   new_expr =
--     {simple} new base_type |
--     {array}  newarray l_paren nonvoid_type r_paren fixed_array_descriptor |
--     {multi}  newmultiarray l_paren base_type r_paren array_descriptor+;
data NewExpr
  = NewSimple Type
  | NewArray Type FixedArrayDescriptor
  | NewMulti Type [ArrayDescriptor] deriving (Eq)
--
--   array_descriptor =
--     l_bracket immediate? r_bracket;
type ArrayDescriptor = Maybe Immediate
--
--   variable =
--     {reference} reference |
--     {local}     local_name;
data Variable
  = VReference Reference
  | VLocal LocalName deriving (Eq)
--
--   bool_expr =
--     {binop} binop_expr |
--     {unop}  unop_expr;
--
--   invoke_expr =
--     {nonstatic} nonstatic_invoke local_name dot method_signature l_paren arg_list? r_paren |
--     {static}    staticinvoke method_signature l_paren arg_list? r_paren |
--     {dynamic}   dynamicinvoke string_constant [dynmethod]:unnamed_method_signature [firstl]:l_paren [dynargs]:arg_list? [firstr]:r_paren
--                                               [bsm]:method_signature l_paren [staticargs]:arg_list? r_paren;
data InvokeExpr
  = SpecialInvoke LocalName MethodSignature (Maybe ArgList)
  | VirtualInvoke LocalName MethodSignature (Maybe ArgList)
  | InterfaceInvoke LocalName MethodSignature (Maybe ArgList)
  | StaticInvoke MethodSignature (Maybe ArgList)
  | DynamicInvoke String UnnamedMethodSignature (Maybe ArgList) MethodSignature (Maybe ArgList) deriving (Eq)
--
--   binop_expr =
--     [left]:immediate binop [right]:immediate;
--
--   unop_expr =
--     unop immediate;
--
--   nonstatic_invoke =
--     {special}   specialinvoke |
--     {virtual}   virtualinvoke |
--     {interface} interfaceinvoke;
--
--   unnamed_method_signature =
--     cmplt type l_paren parameter_list? r_paren cmpgt;
data UnnamedMethodSignature = UnnamedMethodSignature { returnType :: Type
                                                     , parameters :: ParameterList
                                                     } deriving (Eq)
--
--   method_signature =
--     cmplt [class_name]:class_name [first]:colon type [method_name]:name  l_paren parameter_list? r_paren cmpgt;
data MethodSignature = MethodSignature { className :: ClassName
                                       , returnType :: Type
                                       , methodName :: Name
                                       , parameters :: Maybe ParameterList
                                       } deriving (Eq)
--
--   reference =
--     {array} array_ref |
--     {field} field_ref;
data Reference
  = ArrayReference Identifier FixedArrayDescriptor
  | FieldReference LocalName FieldSignature
  | SignatureReference FieldSignature deriving (Eq)
--
--   array_ref =
--     {ident} identifier fixed_array_descriptor |
--     {quoted} quoted_name fixed_array_descriptor;
--
--   field_ref =
--     {local} local_name dot field_signature |
--     {sig}   field_signature;
--
--   field_signature =
--     cmplt [class_name]:class_name [first]:colon type [field_name]:name cmpgt;
data FieldSignature = FieldSignature { className :: ClassName
                                     , fieldType :: Type
                                     , fieldName :: Name
                                     } deriving (Eq)
--
--   fixed_array_descriptor =
--     l_bracket immediate r_bracket;
type FixedArrayDescriptor = Immediate
--
--   arg_list =
--     {single} immediate |
--     {multi}  immediate comma arg_list;
type ArgList = [Immediate]
--
--   immediate =
--     {local}    local_name |
--     {constant} constant;
data Immediate
  = ILocalName String
  | IInt Int
  | IFloat Float
  | IString String
  | IClass String
  | INull deriving (Eq)

--
--   constant =
--     {integer} minus? integer_constant |
--     {float}   minus? float_constant |
--     {string}  string_constant |
--     {clzz}    [id]:class string_constant |
--     {null}    null;
--
--   binop =
--     {and}   and |
--     {or}    or |
--     {xor}   xor |
--     {mod}   mod |
--     {cmp}   cmp |
--     {cmpg}  cmpg |
--     {cmpl}  cmpl |
--     {cmpeq} cmpeq |
--     {cmpne} cmpne |
--     {cmpgt} cmpgt |
--     {cmpge} cmpge |
--     {cmplt} cmplt |
--     {cmple} cmple |
--     {shl}   shl |
--     {shr}   shr |
--     {ushr}  ushr |
--     {plus}  plus |
--     {minus} minus |
--     {mult}  mult |
--     {div}   div;
data Binop
  = And     -- Bytewise operator
  | Or      -- Bytewise operator
  | Xor     -- Bytewise operator
  | Mod
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
--
--   unop =
--     {lengthof} lengthof |
--     {neg}      neg;
data Unop
  = Lengthof
  | Neg deriving (Eq)
--
-- class_name =
--     {quoted} quoted_name |
--     {ident}  identifier |
--     {full_ident} full_identifier;
type ClassName = String
--
-- name =
--     {quoted} quoted_name |
--     {ident}  identifier;
type Name = String

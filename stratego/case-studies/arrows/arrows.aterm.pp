Specification(
  [ Signature(
      [ Constructors(
          [ OpDecl("Nil", ConstType(Sort("List", [SortVar("a")])))
          , OpDecl(
              "Cons"
            , FunType(
                [ConstType(SortVar("a")), ConstType(Sort("List", [SortVar("a")]))]
              , ConstType(Sort("List", [SortVar("a")]))
              )
            )
          , OpDecl(
              "Conc"
            , FunType(
                [ ConstType(Sort("List", [SortVar("a")]))
                , ConstType(Sort("List", [SortVar("a")]))
                ]
              , ConstType(Sort("List", [SortVar("a")]))
              )
            )
          , OpDeclInj(ConstType(SortTuple([])))
          , OpDeclInj(
              FunType(
                [ConstType(SortVar("a"))]
              , ConstType(SortTuple([SortVar("a")]))
              )
            )
          , OpDeclInj(
              FunType(
                [ConstType(SortVar("a")), ConstType(SortVar("b"))]
              , ConstType(SortTuple([SortVar("a"), SortVar("b")]))
              )
            )
          , OpDeclInj(
              FunType(
                [ConstType(SortVar("a")), ConstType(SortVar("b")), ConstType(SortVar("c"))]
              , ConstType(
                  SortTuple([SortVar("a"), SortVar("b"), SortVar("c")])
                )
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Empty"))], ConstType(SortNoArgs("NoOffsideDeclListSem_Empty0")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideDeclListSem"))], ConstType(SortNoArgs("NoOffsideDeclListSem_Empty0")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Empty"))], ConstType(SortNoArgs("OffsideDeclList_Empty0")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideDeclList"))], ConstType(SortNoArgs("OffsideDeclList_Empty0")))
            )
          , OpDecl(
              "Conc"
            , FunType(
                [ConstType(SortNoArgs("ListStarOfCharClass0")), ConstType(SortNoArgs("ListPlusOfCharClass0"))]
              , ConstType(SortNoArgs("ListPlusOfCharClass0"))
              )
            )
          , OpDecl(
              "Conc"
            , FunType(
                [ConstType(SortNoArgs("ListPlusOfCharClass0")), ConstType(SortNoArgs("ListStarOfCharClass0"))]
              , ConstType(SortNoArgs("ListPlusOfCharClass0"))
              )
            )
          , OpDecl(
              "Conc"
            , FunType(
                [ConstType(SortNoArgs("ListPlusOfCharClass0")), ConstType(SortNoArgs("ListPlusOfCharClass0"))]
              , ConstType(SortNoArgs("ListPlusOfCharClass0"))
              )
            )
          , OpDecl(
              "Cons"
            , FunType(
                [ConstType(SortNoArgs("Int")), ConstType(SortNoArgs("ListStarOfCharClass0"))]
              , ConstType(SortNoArgs("ListPlusOfCharClass0"))
              )
            )
          , OpDecl(
              "StmtSeqOff"
            , FunType(
                [ConstType(SortNoArgs("OffsideStmt")), ConstType(SortNoArgs("OffsideStmt"))]
              , ConstType(SortNoArgs("OffsideStmt"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Stmt"))], ConstType(SortNoArgs("OffsideStmt")))
            )
          , OpDecl(
              "DeclSeqOff"
            , FunType(
                [ConstType(SortNoArgs("OffsideDecl")), ConstType(SortNoArgs("Decl"))]
              , ConstType(SortNoArgs("OffsideDecl"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Decl"))], ConstType(SortNoArgs("OffsideDecl")))
            )
          , OpDecl(
              "AltSeqOff"
            , FunType(
                [ConstType(SortNoArgs("OffsideAlt")), ConstType(SortNoArgs("Alt"))]
              , ConstType(SortNoArgs("OffsideAlt"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Alt"))], ConstType(SortNoArgs("OffsideAlt")))
            )
          , OpDecl(
              "TopdeclSeqOff"
            , FunType(
                [ConstType(SortNoArgs("Topdecl")), ConstType(SortNoArgs("OffsideTopdecl"))]
              , ConstType(SortNoArgs("OffsideTopdecl"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Topdecl"))], ConstType(SortNoArgs("OffsideTopdecl")))
            )
          , OpDecl(
              "ImportdeclSeqOff"
            , FunType(
                [ConstType(SortNoArgs("Importdecl")), ConstType(SortNoArgs("OffsideImportdecl"))]
              , ConstType(SortNoArgs("OffsideImportdecl"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Importdecl"))], ConstType(SortNoArgs("OffsideImportdecl")))
            )
          , OpDecl(
              "FloatHash"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Float-HASH")))
            )
          , OpDecl(
              "IntegerHash"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Integer-HASH")))
            )
          , OpDecl(
              "StringHash"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("StringChar")]))]
              , ConstType(SortNoArgs("String-HASH"))
              )
            )
          , OpDecl(
              "CharHash"
            , FunType([ConstType(SortNoArgs("CharChar"))], ConstType(SortNoArgs("Char-HASH")))
            )
          , OpDecl(
              "FlexibleContext"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("FlexibleClass")]))]
              , ConstType(SortNoArgs("FlexibleContext"))
              )
            )
          , OpDecl(
              "FlexibleContext"
            , FunType([ConstType(SortNoArgs("FlexibleClass"))], ConstType(SortNoArgs("FlexibleContext")))
            )
          , OpDecl(
              "SimpleClassFle"
            , FunType(
                [ConstType(SortNoArgs("Qtycls")), ConstType(SortNoArgs("Tyvar"))]
              , ConstType(SortNoArgs("FlexibleClass"))
              )
            )
          , OpDecl(
              "ClassFlex"
            , FunType(
                [ConstType(SortNoArgs("Qtycls")), ConstType(SortNoArgs("Gtycon"))]
              , ConstType(SortNoArgs("FlexibleClass"))
              )
            )
          , OpDecl(
              "ClassFlex"
            , FunType(
                [ConstType(SortNoArgs("Qtycls")), ConstType(SortNoArgs("Type"))]
              , ConstType(SortNoArgs("FlexibleClass"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideStmt"))], ConstType(SortNoArgs("OffsideStmtList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Stmt"))], ConstType(SortNoArgs("NoOffsideStmt")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideStmtList"))], ConstType(SortNoArgs("NoOffsideStmtListSem")))
            )
          , OpDecl(
              "StmtSeq"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideStmt")), ConstType(SortNoArgs("NoOffsideStmtList"))]
              , ConstType(SortNoArgs("NoOffsideStmtList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideStmt"))], ConstType(SortNoArgs("NoOffsideStmtList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideStmtListSem"))], ConstType(SortNoArgs("NoOffsideStmtBlock")))
            )
          , OpDecl(
              "StmtList"
            , FunType([ConstType(SortNoArgs("OffsideStmtList"))], ConstType(SortNoArgs("StmtList")))
            )
          , OpDecl(
              "StmtList"
            , FunType([ConstType(SortNoArgs("NoOffsideStmtBlock"))], ConstType(SortNoArgs("StmtList")))
            )
          , OpDecl(
              "FBind"
            , FunType(
                [ConstType(SortNoArgs("Qvar")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Fbind"))
              )
            )
          , OpDecl(
              "LetStmt"
            , FunType([ConstType(SortNoArgs("Declbinds"))], ConstType(SortNoArgs("Stmt")))
            )
          , OpDecl(
              "ExpStmt"
            , FunType([ConstType(SortNoArgs("Exp"))], ConstType(SortNoArgs("Stmt")))
            )
          , OpDecl(
              "BindStmt"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Stmt"))
              )
            )
          , OpDecl(
              "ListCompr"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(Sort("List", [SortNoArgs("Qual")]))]
              , ConstType(SortNoArgs("List"))
              )
            )
          , OpDecl(
              "ListFirstFromTo"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("List"))
              )
            )
          , OpDecl(
              "ListFromTo"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("List"))
              )
            )
          , OpDecl(
              "ListFirstFrom"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("List"))
              )
            )
          , OpDecl(
              "ListFrom"
            , FunType([ConstType(SortNoArgs("Exp"))], ConstType(SortNoArgs("List")))
            )
          , OpDecl(
              "List"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Exp")]))]
              , ConstType(SortNoArgs("List"))
              )
            )
          , OpDecl(
              "QualLet"
            , FunType([ConstType(SortNoArgs("Declbinds"))], ConstType(SortNoArgs("Qual")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Exp"))], ConstType(SortNoArgs("Qual")))
            )
          , OpDecl(
              "QualBind"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Qual"))
              )
            )
          , OpDecl(
              "PatBind"
            , FunType(
                [ConstType(SortNoArgs("Qvar")), ConstType(SortNoArgs("Pat"))]
              , ConstType(SortNoArgs("FPat"))
              )
            )
          , OpDecl(
              "LabeledPats"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("FPat")]))]
              , ConstType(SortNoArgs("LabeledPat"))
              )
            )
          , OpDecl(
              "Irrefutable"
            , FunType([ConstType(SortNoArgs("APat"))], ConstType(SortNoArgs("APat")))
            )
          , OpDecl(
              "ListPat"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Pat")]))]
              , ConstType(SortNoArgs("APat"))
              )
            )
          , OpDecl(
              "TuplePat"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(Sort("List", [SortNoArgs("Pat")]))]
              , ConstType(SortNoArgs("APat"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Pat"))], ConstType(SortNoArgs("APat")))
            )
          , OpDecl("Wildcard", ConstType(SortNoArgs("APat")))
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Literal"))], ConstType(SortNoArgs("APat")))
            )
          , OpDecl(
              "LabeledPat"
            , FunType(
                [ConstType(SortNoArgs("Qcon")), ConstType(SortNoArgs("LabeledPat"))]
              , ConstType(SortNoArgs("APat"))
              )
            )
          , OpDecl(
              "ConstrPat"
            , FunType([ConstType(SortNoArgs("Gcon"))], ConstType(SortNoArgs("APat")))
            )
          , OpDecl(
              "NamedPat"
            , FunType(
                [ConstType(SortNoArgs("Var")), ConstType(SortNoArgs("APat"))]
              , ConstType(SortNoArgs("APat"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Var"))], ConstType(SortNoArgs("APat")))
            )
          , OpDecl(
              "ConstrApp"
            , FunType(
                [ConstType(SortNoArgs("Gcon")), ConstType(Sort("List", [SortNoArgs("APat")]))]
              , ConstType(SortNoArgs("LPat"))
              )
            )
          , OpDecl(
              "NegationPat"
            , FunType([ConstType(SortNoArgs("Literal"))], ConstType(SortNoArgs("LPat")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("APat"))], ConstType(SortNoArgs("LPat")))
            )
          , OpDecl(
              "BinOpApp"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("Qconop")), ConstType(SortNoArgs("LPat"))]
              , ConstType(SortNoArgs("Pat"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("LPat"))], ConstType(SortNoArgs("Pat")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideDecl"))], ConstType(SortNoArgs("OffsideDeclList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Decl"))], ConstType(SortNoArgs("NoOffsideDecl")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideDeclList"))], ConstType(SortNoArgs("NoOffsideDeclListSem")))
            )
          , OpDecl(
              "DeclSeq"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideDecl")), ConstType(SortNoArgs("NoOffsideDeclList"))]
              , ConstType(SortNoArgs("NoOffsideDeclList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideDecl"))], ConstType(SortNoArgs("NoOffsideDeclList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideDeclListSem_Empty0"))], ConstType(SortNoArgs("NoOffsideDeclBlock")))
            )
          , OpDecl(
              "DeclList"
            , FunType([ConstType(SortNoArgs("OffsideDeclList_Empty0"))], ConstType(SortNoArgs("DeclList")))
            )
          , OpDecl(
              "DeclList"
            , FunType([ConstType(SortNoArgs("NoOffsideDeclBlock"))], ConstType(SortNoArgs("DeclList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("DeclList"))], ConstType(SortNoArgs("Declbinds")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Where"))], ConstType(SortNoArgs("MaybeWhere")))
            )
          , OpDecl(
              "Where"
            , FunType([ConstType(SortNoArgs("DeclList"))], ConstType(SortNoArgs("Where")))
            )
          , OpDecl(
              "NestedFunLHS"
            , FunType(
                [ConstType(SortNoArgs("FunLHS")), ConstType(Sort("List", [SortNoArgs("APat")]))]
              , ConstType(SortNoArgs("FunLHS"))
              )
            )
          , OpDecl(
              "OpFunLHS"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("Varop")), ConstType(SortNoArgs("Pat"))]
              , ConstType(SortNoArgs("FunLHS"))
              )
            )
          , OpDecl(
              "VarFunLHS"
            , FunType(
                [ConstType(SortNoArgs("Var")), ConstType(Sort("List", [SortNoArgs("APat")]))]
              , ConstType(SortNoArgs("FunLHS"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Pat"))], ConstType(SortNoArgs("FunLHS")))
            )
          , OpDecl(
              "Guarded"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Gdrh"))
              )
            )
          , OpDecl(
              "GdValdef"
            , FunType(
                [ ConstType(SortNoArgs("FunLHS"))
                , ConstType(Sort("List", [SortNoArgs("Gdrh")]))
                , ConstType(SortNoArgs("MaybeWhere"))
                ]
              , ConstType(SortNoArgs("Valdef"))
              )
            )
          , OpDecl(
              "Valdef"
            , FunType(
                [ConstType(SortNoArgs("FunLHS")), ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("MaybeWhere"))]
              , ConstType(SortNoArgs("Valdef"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideAlt"))], ConstType(SortNoArgs("OffsideAltList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Alt"))], ConstType(SortNoArgs("NoOffsideAlt")))
            )
          , OpDecl(
              "AltSeq"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideAlt")), ConstType(SortNoArgs("NoOffsideAltList"))]
              , ConstType(SortNoArgs("NoOffsideAltList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideAlt"))], ConstType(SortNoArgs("NoOffsideAltList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideAltList"))], ConstType(SortNoArgs("NoOffsideAltBlock")))
            )
          , OpDecl(
              "AltList"
            , FunType([ConstType(SortNoArgs("OffsideAltList"))], ConstType(SortNoArgs("AltList")))
            )
          , OpDecl(
              "AltList"
            , FunType([ConstType(SortNoArgs("NoOffsideAltBlock"))], ConstType(SortNoArgs("AltList")))
            )
          , OpDecl(
              "GdPat"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Gdpat"))
              )
            )
          , OpDecl(
              "GdAlt"
            , FunType(
                [ ConstType(SortNoArgs("Pat"))
                , ConstType(Sort("List", [SortNoArgs("Gdpat")]))
                , ConstType(SortNoArgs("MaybeWhere"))
                ]
              , ConstType(SortNoArgs("Alt"))
              )
            )
          , OpDecl(
              "Alt"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("MaybeWhere"))]
              , ConstType(SortNoArgs("Alt"))
              )
            )
          , OpDecl(
              "LabelBinds"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Fbind")]))]
              , ConstType(SortNoArgs("LabelBinds"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qop"))], ConstType(SortNoArgs("QopNoNeg")))
            )
          , OpDecl(
              "FixDecl"
            , FunType(
                [ConstType(SortNoArgs("Infix")), ConstType(SortNoArgs("Prec")), ConstType(SortNoArgs("Ops"))]
              , ConstType(SortNoArgs("Fixdecl"))
              )
            )
          , OpDeclInj(
              FunType(
                [ConstType(Sort("List", [SortNoArgs("Op")]))]
              , ConstType(SortNoArgs("Ops"))
              )
            )
          , OpDeclInj(
              FunType(
                [ConstType(Sort("Option", [SortNoArgs("INTEGER")]))]
              , ConstType(SortNoArgs("Prec"))
              )
            )
          , OpDecl("InfixR", ConstType(SortNoArgs("Infix")))
          , OpDecl("InfixL", ConstType(SortNoArgs("Infix")))
          , OpDecl("Infix", ConstType(SortNoArgs("Infix")))
          , OpDeclInj(
              FunType(
                [ConstType(Sort("List", [SortNoArgs("APat")]))]
              , ConstType(SortNoArgs("Fargs"))
              )
            )
          , OpDecl(
              "ECons"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(Sort("List", [SortNoArgs("Exp")]))]
              , ConstType(SortNoArgs("Exps2"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Exp"))], ConstType(SortNoArgs("AnyExp")))
            )
          , OpDecl(
              "ArrOpApp"
            , FunType(
                [ConstType(SortNoArgs("ArrCommand")), ConstType(SortNoArgs("Qop")), ConstType(SortNoArgs("ArrCommand"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrForm"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(Sort("List", [SortNoArgs("ArrCommand")]))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrAppBin"
            , FunType(
                [ConstType(SortNoArgs("ArrCommand")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrDo"
            , FunType([ConstType(SortNoArgs("ArrStmtList"))], ConstType(SortNoArgs("ArrCommand")))
            )
          , OpDecl(
              "ArrCase"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("ArrAltList"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrIf"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("ArrCommand")), ConstType(SortNoArgs("ArrCommand"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrLet"
            , FunType(
                [ConstType(SortNoArgs("Declbinds")), ConstType(SortNoArgs("ArrCommand"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrAbs"
            , FunType(
                [ConstType(SortNoArgs("Fargs")), ConstType(SortNoArgs("ArrCommand"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrHigher"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "ArrFirst"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("ArrCommand"))
              )
            )
          , OpDecl(
              "Typed"
            , FunType(
                [ ConstType(SortNoArgs("Exp"))
                , ConstType(Sort("Option", [SortNoArgs("Context")]))
                , ConstType(SortNoArgs("Type"))
                ]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Negation"
            , FunType([ConstType(SortNoArgs("Exp"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "Labeled"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("LabelBinds"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Named"
            , FunType(
                [ConstType(SortNoArgs("Qvar")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "OpApp"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Qop")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "AppBin"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("List"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "Case"
            , FunType(
                [ConstType(SortNoArgs("AnyExp")), ConstType(SortNoArgs("AltList"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Do"
            , FunType([ConstType(SortNoArgs("StmtList"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "If"
            , FunType(
                [ConstType(SortNoArgs("AnyExp")), ConstType(SortNoArgs("AnyExp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Let"
            , FunType(
                [ConstType(SortNoArgs("Declbinds")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Abs"
            , FunType(
                [ConstType(SortNoArgs("Fargs")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "RSection"
            , FunType(
                [ConstType(SortNoArgs("QopNoNeg")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "LSection"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Qop"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Product"
            , FunType([ConstType(SortNoArgs("Exps2"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "Lit"
            , FunType([ConstType(SortNoArgs("Literal"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "Constr"
            , FunType([ConstType(SortNoArgs("Gcon"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "QVar"
            , FunType([ConstType(SortNoArgs("Qvar"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "ArrProcedure"
            , FunType(
                [ConstType(SortNoArgs("APat")), ConstType(SortNoArgs("ArrCommand"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "ArrStmtSeq"
            , FunType(
                [ConstType(SortNoArgs("ArrStmt")), ConstType(SortNoArgs("ArrExplStmtList"))]
              , ConstType(SortNoArgs("ArrExplStmtList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("ArrStmt"))], ConstType(SortNoArgs("ArrExplStmtList")))
            )
          , OpDecl(
              "ArrStmtList"
            , FunType([ConstType(SortNoArgs("ArrImplStmtList"))], ConstType(SortNoArgs("ArrStmtList")))
            )
          , OpDecl(
              "ArrStmtList"
            , FunType([ConstType(SortNoArgs("ArrExplStmtList"))], ConstType(SortNoArgs("ArrStmtList")))
            )
          , OpDecl(
              "ArrCmdStmt"
            , FunType([ConstType(SortNoArgs("ArrCommand"))], ConstType(SortNoArgs("ArrStmt")))
            )
          , OpDecl(
              "ArrBindStmt"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("ArrCommand"))]
              , ConstType(SortNoArgs("ArrStmt"))
              )
            )
          , OpDecl(
              "ArrLetStmt"
            , FunType([ConstType(SortNoArgs("Declbinds"))], ConstType(SortNoArgs("ArrStmt")))
            )
          , OpDecl(
              "ArrAltSeqOff"
            , FunType(
                [ConstType(SortNoArgs("ArrOffsideAlt")), ConstType(SortNoArgs("ArrOffsideAltList"))]
              , ConstType(SortNoArgs("ArrOffsideAltList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("ArrOffsideAlt"))], ConstType(SortNoArgs("ArrOffsideAltList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("ArrAlt"))], ConstType(SortNoArgs("ArrOffsideAlt")))
            )
          , OpDecl(
              "ArrAltSeq"
            , FunType(
                [ConstType(SortNoArgs("ArrAlt")), ConstType(SortNoArgs("ArrNoOffsideAltList"))]
              , ConstType(SortNoArgs("ArrNoOffsideAltList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("ArrAlt"))], ConstType(SortNoArgs("ArrNoOffsideAltList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("ArrNoOffsideAltList"))], ConstType(SortNoArgs("ArrNoOffsideAltBlock")))
            )
          , OpDecl(
              "ArrAltList"
            , FunType([ConstType(SortNoArgs("ArrOffsideAltList"))], ConstType(SortNoArgs("ArrAltList")))
            )
          , OpDecl(
              "ArrAltList"
            , FunType([ConstType(SortNoArgs("ArrNoOffsideAltBlock"))], ConstType(SortNoArgs("ArrAltList")))
            )
          , OpDecl(
              "ArrAlt"
            , FunType(
                [ConstType(SortNoArgs("Pat")), ConstType(SortNoArgs("ArrCommand")), ConstType(SortNoArgs("MaybeWhere"))]
              , ConstType(SortNoArgs("ArrAlt"))
              )
            )
          , OpDecl(
              "SignDecl"
            , FunType(
                [ ConstType(SortNoArgs("Vars"))
                , ConstType(Sort("Option", [SortNoArgs("Context")]))
                , ConstType(SortNoArgs("Type"))
                ]
              , ConstType(SortNoArgs("Signdecl"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Valdef"))], ConstType(SortNoArgs("Decl")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Fixdecl"))], ConstType(SortNoArgs("Decl")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Signdecl"))], ConstType(SortNoArgs("Decl")))
            )
          , OpDecl(
              "ClassMulti"
            , FunType(
                [ ConstType(SortNoArgs("Qtycls"))
                , ConstType(SortNoArgs("Tyvar"))
                , ConstType(Sort("List", [SortNoArgs("AType")]))
                ]
              , ConstType(SortNoArgs("Class"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("SimpleClass"))], ConstType(SortNoArgs("Class")))
            )
          , OpDecl(
              "SimpleClass"
            , FunType(
                [ConstType(SortNoArgs("Qtycls")), ConstType(SortNoArgs("Tyvar"))]
              , ConstType(SortNoArgs("SimpleClass"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("FlexibleContext"))], ConstType(SortNoArgs("SContext")))
            )
          , OpDecl(
              "SContext"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("SimpleClass")]))]
              , ConstType(SortNoArgs("SContext"))
              )
            )
          , OpDecl(
              "SContext"
            , FunType([ConstType(SortNoArgs("SimpleClass"))], ConstType(SortNoArgs("SContext")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("FlexibleContext"))], ConstType(SortNoArgs("Context")))
            )
          , OpDecl(
              "Context"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Class")]))]
              , ConstType(SortNoArgs("Context"))
              )
            )
          , OpDecl(
              "Context"
            , FunType([ConstType(SortNoArgs("Class"))], ConstType(SortNoArgs("Context")))
            )
          , OpDecl(
              "InstArrow"
            , FunType(
                [ConstType(SortNoArgs("Tyvar")), ConstType(SortNoArgs("Tyvar"))]
              , ConstType(SortNoArgs("Inst"))
              )
            )
          , OpDecl(
              "InstList"
            , FunType([ConstType(SortNoArgs("Tyvar"))], ConstType(SortNoArgs("Inst")))
            )
          , OpDecl(
              "InstTuple"
            , FunType(
                [ConstType(SortNoArgs("Tyvar")), ConstType(Sort("List", [SortNoArgs("Tyvar")]))]
              , ConstType(SortNoArgs("Inst"))
              )
            )
          , OpDecl(
              "InstApp"
            , FunType(
                [ConstType(SortNoArgs("Gtycon")), ConstType(Sort("List", [SortNoArgs("Tyvar")]))]
              , ConstType(SortNoArgs("Inst"))
              )
            )
          , OpDecl(
              "InstCons"
            , FunType([ConstType(SortNoArgs("Gtycon"))], ConstType(SortNoArgs("Inst")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Type"))], ConstType(SortNoArgs("Sbtype")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("AType"))], ConstType(SortNoArgs("Satype")))
            )
          , OpDecl(
              "InfixConstr"
            , FunType(
                [ConstType(SortNoArgs("Sbtype")), ConstType(SortNoArgs("Conop")), ConstType(SortNoArgs("Sbtype"))]
              , ConstType(SortNoArgs("Constr"))
              )
            )
          , OpDecl(
              "ConstrDecl"
            , FunType(
                [ConstType(SortNoArgs("Conid")), ConstType(Sort("List", [SortNoArgs("Satype")]))]
              , ConstType(SortNoArgs("Constr"))
              )
            )
          , OpDecl(
              "ConstrDecls"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Constr")]))]
              , ConstType(SortNoArgs("Constrs"))
              )
            )
          , OpDecl("NoConstrDecls", ConstType(SortNoArgs("Constrs")))
          , OpDecl(
              "Derive"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Qtycls")]))]
              , ConstType(SortNoArgs("Deriving"))
              )
            )
          , OpDecl("NoDeriving", ConstType(SortNoArgs("Deriving")))
          , OpDecl(
              "Derive"
            , FunType([ConstType(SortNoArgs("Qtycls"))], ConstType(SortNoArgs("Deriving")))
            )
          , OpDecl(
              "TFunBin"
            , FunType(
                [ConstType(SortNoArgs("Type")), ConstType(SortNoArgs("Type"))]
              , ConstType(SortNoArgs("Type"))
              )
            )
          , OpDecl(
              "TAppBin"
            , FunType(
                [ConstType(SortNoArgs("Type")), ConstType(SortNoArgs("Type"))]
              , ConstType(SortNoArgs("Type"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("AType"))], ConstType(SortNoArgs("Type")))
            )
          , OpDecl(
              "TProd"
            , FunType([ConstType(SortNoArgs("Types2"))], ConstType(SortNoArgs("AType")))
            )
          , OpDecl(
              "TList"
            , FunType([ConstType(SortNoArgs("Type"))], ConstType(SortNoArgs("AType")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Type"))], ConstType(SortNoArgs("AType")))
            )
          , OpDecl(
              "TVar"
            , FunType([ConstType(SortNoArgs("Tyvar"))], ConstType(SortNoArgs("AType")))
            )
          , OpDecl(
              "TCon"
            , FunType([ConstType(SortNoArgs("Gtycon"))], ConstType(SortNoArgs("AType")))
            )
          , OpDecl(
              "TCons"
            , FunType(
                [ConstType(SortNoArgs("Type")), ConstType(Sort("List", [SortNoArgs("Type")]))]
              , ConstType(SortNoArgs("Types2"))
              )
            )
          , OpDecl("TListCon", ConstType(SortNoArgs("Gtycon")))
          , OpDecl("TUnit", ConstType(SortNoArgs("Gtycon")))
          , OpDecl("TArrow", ConstType(SortNoArgs("Gtycon")))
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qtycon"))], ConstType(SortNoArgs("Gtycon")))
            )
          , OpDecl(
              "Hiding"
            , FunType([ConstType(SortNoArgs("Exportlist"))], ConstType(SortNoArgs("Impspec")))
            )
          , OpDecl(
              "Impspec"
            , FunType([ConstType(SortNoArgs("Exportlist"))], ConstType(SortNoArgs("Impspec")))
            )
          , OpDecl(
              "As"
            , FunType([ConstType(SortNoArgs("Modid"))], ConstType(SortNoArgs("As")))
            )
          , OpDecl("Qualified", ConstType(SortNoArgs("Qualified")))
          , OpDecl("SOURCE", ConstType(SortNoArgs("Src")))
          , OpDecl(
              "Import"
            , FunType(
                [ ConstType(Sort("Option", [SortNoArgs("Src")]))
                , ConstType(Sort("Option", [SortNoArgs("Qualified")]))
                , ConstType(SortNoArgs("Modid"))
                , ConstType(Sort("Option", [SortNoArgs("As")]))
                , ConstType(Sort("Option", [SortNoArgs("Impspec")]))
                ]
              , ConstType(SortNoArgs("Importdecl"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Gtycon"))], ConstType(SortNoArgs("Export")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qvar"))], ConstType(SortNoArgs("Export")))
            )
          , OpDecl(
              "Exports"
            , FunType([ConstType(SortNoArgs("Exportlist"))], ConstType(SortNoArgs("Exports")))
            )
          , OpDecl(
              "Exportlist"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Export")]))]
              , ConstType(SortNoArgs("Exportlist"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideTopdecl"))], ConstType(SortNoArgs("OffsideTopdeclList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideImportdecl"))], ConstType(SortNoArgs("OffsideImportdeclList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Topdecl"))], ConstType(SortNoArgs("NoOffsideTopdecl")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideTopdeclList"))], ConstType(SortNoArgs("NoOffsideTopdeclListSem")))
            )
          , OpDecl(
              "TopdeclSeq"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideTopdecl")), ConstType(SortNoArgs("NoOffsideTopdeclList"))]
              , ConstType(SortNoArgs("NoOffsideTopdeclList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideTopdecl"))], ConstType(SortNoArgs("NoOffsideTopdeclList")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Importdecl"))], ConstType(SortNoArgs("NoOffsideImportdecl")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideImportdeclList"))], ConstType(SortNoArgs("NoOffsideImportdeclListSem")))
            )
          , OpDecl(
              "ImportdeclSeq"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideImportdecl")), ConstType(SortNoArgs("NoOffsideImportdeclList"))]
              , ConstType(SortNoArgs("NoOffsideImportdeclList"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideImportdecl"))], ConstType(SortNoArgs("NoOffsideImportdeclList")))
            )
          , OpDecl(
              "OffBody"
            , FunType(
                [ConstType(SortNoArgs("OffsideImportdeclList")), ConstType(SortNoArgs("Empty"))]
              , ConstType(SortNoArgs("OffsideBody"))
              )
            )
          , OpDecl(
              "OffBody"
            , FunType(
                [ConstType(SortNoArgs("Empty")), ConstType(SortNoArgs("OffsideTopdeclList"))]
              , ConstType(SortNoArgs("OffsideBody"))
              )
            )
          , OpDecl(
              "OffBody"
            , FunType(
                [ConstType(SortNoArgs("Empty")), ConstType(SortNoArgs("Empty"))]
              , ConstType(SortNoArgs("OffsideBody"))
              )
            )
          , OpDecl(
              "Body"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideImportdeclListSem")), ConstType(SortNoArgs("NoOffsideTopdeclList"))]
              , ConstType(SortNoArgs("NoOffsideBody"))
              )
            )
          , OpDecl(
              "Body"
            , FunType(
                [ConstType(SortNoArgs("NoOffsideImportdeclListSem")), ConstType(SortNoArgs("Empty"))]
              , ConstType(SortNoArgs("NoOffsideBody"))
              )
            )
          , OpDecl(
              "Body"
            , FunType(
                [ConstType(SortNoArgs("Empty")), ConstType(SortNoArgs("NoOffsideTopdeclListSem"))]
              , ConstType(SortNoArgs("NoOffsideBody"))
              )
            )
          , OpDecl(
              "Body"
            , FunType(
                [ConstType(SortNoArgs("Empty")), ConstType(SortNoArgs("Empty"))]
              , ConstType(SortNoArgs("NoOffsideBody"))
              )
            )
          , OpDecl("Empty", ConstType(SortNoArgs("Empty")))
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("OffsideBody"))], ConstType(SortNoArgs("Body")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("NoOffsideBody"))], ConstType(SortNoArgs("Body")))
            )
          , OpDecl(
              "FlexibleInstance"
            , FunType(
                [ ConstType(Sort("Option", [SortNoArgs("SContext")]))
                , ConstType(SortNoArgs("Qtycls"))
                , ConstType(Sort("List", [SortNoArgs("AType")]))
                , ConstType(SortNoArgs("MaybeWhere"))
                ]
              , ConstType(SortNoArgs("Topdecl"))
              )
            )
          , OpDecl(
              "Default"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("Type")]))]
              , ConstType(SortNoArgs("Topdecl"))
              )
            )
          , OpDecl(
              "Instance"
            , FunType(
                [ ConstType(Sort("Option", [SortNoArgs("SContext")]))
                , ConstType(SortNoArgs("Qtycls"))
                , ConstType(Sort("List", [SortNoArgs("Inst")]))
                , ConstType(SortNoArgs("MaybeWhere"))
                ]
              , ConstType(SortNoArgs("Topdecl"))
              )
            )
          , OpDecl(
              "Class"
            , FunType(
                [ ConstType(Sort("Option", [SortNoArgs("SContext")]))
                , ConstType(SortNoArgs("Tycls"))
                , ConstType(SortNoArgs("Tyvar"))
                , ConstType(SortNoArgs("MaybeWhere"))
                ]
              , ConstType(SortNoArgs("Topdecl"))
              )
            )
          , OpDecl(
              "Data"
            , FunType(
                [ ConstType(Sort("Option", [SortNoArgs("Context")]))
                , ConstType(SortNoArgs("Type"))
                , ConstType(SortNoArgs("Constrs"))
                , ConstType(SortNoArgs("Deriving"))
                ]
              , ConstType(SortNoArgs("Topdecl"))
              )
            )
          , OpDecl(
              "TypeDecl"
            , FunType(
                [ ConstType(SortNoArgs("Tycon"))
                , ConstType(Sort("List", [SortNoArgs("Tyvar")]))
                , ConstType(SortNoArgs("Type"))
                ]
              , ConstType(SortNoArgs("Topdecl"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Decl"))], ConstType(SortNoArgs("Topdecl")))
            )
          , OpDecl(
              "Program"
            , FunType([ConstType(SortNoArgs("Body"))], ConstType(SortNoArgs("Module")))
            )
          , OpDecl(
              "Module"
            , FunType(
                [ConstType(SortNoArgs("ModuleDec")), ConstType(SortNoArgs("Body"))]
              , ConstType(SortNoArgs("Module"))
              )
            )
          , OpDecl(
              "ModuleDec"
            , FunType(
                [ConstType(SortNoArgs("Modid")), ConstType(Sort("Option", [SortNoArgs("Exports")]))]
              , ConstType(SortNoArgs("ModuleDec"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Float-HASH"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Integer-HASH"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String-HASH"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Char-HASH"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "CLitLit"
            , FunType([ConstType(SortNoArgs("CLITLIT"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "PrimDouble"
            , FunType([ConstType(SortNoArgs("PRIMDOUBLE"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "PrimFloat"
            , FunType([ConstType(SortNoArgs("PRIMFLOAT"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "PrimString"
            , FunType([ConstType(SortNoArgs("PRIMSTRING"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "PrimChar"
            , FunType([ConstType(SortNoArgs("PRIMCHAR"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "PrimInt"
            , FunType([ConstType(SortNoArgs("PRIMINTEGER"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "Float"
            , FunType([ConstType(SortNoArgs("RATIONAL"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "Float"
            , FunType([ConstType(SortNoArgs("FLOAT"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Char"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "Int"
            , FunType([ConstType(SortNoArgs("INTEGER"))], ConstType(SortNoArgs("Literal")))
            )
          , OpDecl(
              "HexadecimalEsc"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Escape")))
            )
          , OpDecl(
              "OctalEsc"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Escape")))
            )
          , OpDecl(
              "DecimalEsc"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Escape")))
            )
          , OpDecl(
              "ASCIIEsc"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Escape")))
            )
          , OpDecl(
              "CharEsc"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Escape")))
            )
          , OpDecl(
              "Gap"
            , FunType([ConstType(SortNoArgs("ListPlusOfCharClass0"))], ConstType(SortNoArgs("StringChar")))
            )
          , OpDecl(
              "EscapeString"
            , FunType([ConstType(SortNoArgs("Escape"))], ConstType(SortNoArgs("StringChar")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("StringChar")))
            )
          , OpDecl(
              "Escape"
            , FunType([ConstType(SortNoArgs("Escape"))], ConstType(SortNoArgs("CharChar")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("CharChar")))
            )
          , OpDecl(
              "String"
            , FunType(
                [ConstType(Sort("List", [SortNoArgs("StringChar")]))]
              , ConstType(SortNoArgs("String"))
              )
            )
          , OpDecl(
              "Char"
            , FunType([ConstType(SortNoArgs("CharChar"))], ConstType(SortNoArgs("Char")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("CLITLIT")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("PRIMDOUBLE")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("PRIMFLOAT")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("PRIMINTEGER")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("PRIMSTRING")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("PRIMCHAR")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("RATIONAL")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("FLOAT")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("INTEGER")))
            )
          , OpDecl(
              "QModId"
            , FunType(
                [ConstType(SortNoArgs("String")), ConstType(SortNoArgs("QModid"))]
              , ConstType(SortNoArgs("QModid"))
              )
            )
          , OpDecl(
              "QModId"
            , FunType(
                [ConstType(SortNoArgs("String")), ConstType(SortNoArgs("String"))]
              , ConstType(SortNoArgs("QModid"))
              )
            )
          , OpDecl(
              "QConSym"
            , FunType(
                [ConstType(SortNoArgs("Modid")), ConstType(SortNoArgs("String"))]
              , ConstType(SortNoArgs("QCONSYM"))
              )
            )
          , OpDecl(
              "QVarSym"
            , FunType(
                [ConstType(SortNoArgs("Modid")), ConstType(SortNoArgs("String"))]
              , ConstType(SortNoArgs("QVARSYM"))
              )
            )
          , OpDecl(
              "QConId"
            , FunType(
                [ConstType(SortNoArgs("Modid")), ConstType(SortNoArgs("String"))]
              , ConstType(SortNoArgs("QCONID"))
              )
            )
          , OpDecl(
              "QVarId"
            , FunType(
                [ConstType(SortNoArgs("Modid")), ConstType(SortNoArgs("String"))]
              , ConstType(SortNoArgs("QVARID"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("QModid"))], ConstType(SortNoArgs("Modid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Modid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("QCONID"))], ConstType(SortNoArgs("Qconid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Conid"))], ConstType(SortNoArgs("Qconid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("CONID"))], ConstType(SortNoArgs("Conid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qtycon"))], ConstType(SortNoArgs("Qtycls")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Tycon"))], ConstType(SortNoArgs("Tycls")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("QCONID"))], ConstType(SortNoArgs("Qtycon")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Tycon"))], ConstType(SortNoArgs("Qtycon")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("CONID"))], ConstType(SortNoArgs("Tycon")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("QVARSYM"))], ConstType(SortNoArgs("Qvarsym1")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("VARSYM"))], ConstType(SortNoArgs("Varsym")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qconid"))], ConstType(SortNoArgs("Qcon")))
            )
          , OpDecl(
              "BinCon"
            , FunType([ConstType(SortNoArgs("Qconsym"))], ConstType(SortNoArgs("Qcon")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("CONSYM"))], ConstType(SortNoArgs("Consym")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("QCONSYM"))], ConstType(SortNoArgs("Qconsym")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Consym"))], ConstType(SortNoArgs("Qconsym")))
            )
          , OpDecl(
              "ConsOp"
            , FunType([ConstType(SortNoArgs("CONSOP"))], ConstType(SortNoArgs("ConsOp")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("ConsOp"))], ConstType(SortNoArgs("Gconsym")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qconsym"))], ConstType(SortNoArgs("Gconsym")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qconop"))], ConstType(SortNoArgs("Qop")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qvarop"))], ConstType(SortNoArgs("Qop")))
            )
          , OpDecl(
              "QPrefCon"
            , FunType([ConstType(SortNoArgs("Qconid"))], ConstType(SortNoArgs("Qconop")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Gconsym"))], ConstType(SortNoArgs("Qconop")))
            )
          , OpDecl(
              "PrefCon"
            , FunType([ConstType(SortNoArgs("Conid"))], ConstType(SortNoArgs("Conop")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Consym"))], ConstType(SortNoArgs("Conop")))
            )
          , OpDecl(
              "QPrefOp"
            , FunType([ConstType(SortNoArgs("Qvarid"))], ConstType(SortNoArgs("Qvarop")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qvarsym"))], ConstType(SortNoArgs("Qvarop")))
            )
          , OpDecl(
              "PrefOp"
            , FunType([ConstType(SortNoArgs("Varid"))], ConstType(SortNoArgs("Varop")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Varsym"))], ConstType(SortNoArgs("Varop")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qvarsym1"))], ConstType(SortNoArgs("Qvarsym")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Varsym"))], ConstType(SortNoArgs("Qvarsym")))
            )
          , OpDecl(
              "ConOp"
            , FunType([ConstType(SortNoArgs("Conop"))], ConstType(SortNoArgs("Op")))
            )
          , OpDecl(
              "Op"
            , FunType([ConstType(SortNoArgs("Varop"))], ConstType(SortNoArgs("Op")))
            )
          , OpDecl(
              "BinOpQ"
            , FunType([ConstType(SortNoArgs("Qvarsym"))], ConstType(SortNoArgs("Qvar")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qvarid"))], ConstType(SortNoArgs("Qvar")))
            )
          , OpDecl(
              "BinOp"
            , FunType([ConstType(SortNoArgs("Varsym"))], ConstType(SortNoArgs("Var")))
            )
          , OpDecl(
              "Var"
            , FunType([ConstType(SortNoArgs("Varid"))], ConstType(SortNoArgs("Var")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("QVARID"))], ConstType(SortNoArgs("Qvarid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Varid"))], ConstType(SortNoArgs("Qvarid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("Qcon"))], ConstType(SortNoArgs("Gcon")))
            )
          , OpDecl("EmptyList", ConstType(SortNoArgs("Gcon")))
          , OpDecl("Unit", ConstType(SortNoArgs("Gcon")))
          , OpDecl(
              "Ins"
            , FunType([ConstType(SortNoArgs("Qvar"))], ConstType(SortNoArgs("Vars")))
            )
          , OpDecl(
              "Snoc"
            , FunType(
                [ConstType(SortNoArgs("Vars")), ConstType(SortNoArgs("Var"))]
              , ConstType(SortNoArgs("Vars"))
              )
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Tyvar")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Varid")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("CONSOP")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("CONSYM")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("VARSYM")))
            )
          , OpDeclInj(
              FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("CONID")))
            )
          ]
        )
      ]
    )
  , Strategies(
      [ SDefT(
          "desugar_arrow_0_0"
        , []
        , []
        , Scope(
            ["p_12", "q_12", "r_12", "s_12", "t_12", "v_12", "u_12", "w_12"]
          , Seq(
              Match(
                Anno(
                  Op("ArrProcedure", [Var("q_12"), Var("p_12")])
                , Wld()
                )
              )
            , Seq(
                Match(Var("s_12"))
              , Seq(
                  Build(Var("q_12"))
                , Seq(
                    CallT(SVar("free_pat_vars_0_0"), [], [])
                  , Seq(
                      Match(Var("r_12"))
                    , Seq(
                        Build(Var("s_12"))
                      , Seq(
                          Match(Var("v_12"))
                        , Seq(
                            Build(Var("r_12"))
                          , Seq(
                              CallT(SVar("tuple_0_0"), [], [])
                            , Seq(
                                Match(Var("t_12"))
                              , Seq(
                                  Build(Var("v_12"))
                                , Seq(
                                    Match(Var("w_12"))
                                  , Seq(
                                      Build(Var("p_12"))
                                    , Seq(
                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("r_12")])
                                      , Seq(
                                          Match(Var("u_12"))
                                        , Seq(
                                            Build(Var("w_12"))
                                          , Build(
                                              Anno(
                                                Op(
                                                  "OpApp"
                                                , [ Anno(
                                                      Op(
                                                        "AppBin"
                                                      , [ Anno(
                                                            Op(
                                                              "Var"
                                                            , [Anno(Str("arr"), Op("Nil", []))]
                                                            )
                                                          , Op("Nil", [])
                                                          )
                                                        , Anno(
                                                            Op(
                                                              "Abs"
                                                            , [ Anno(
                                                                  Op(
                                                                    "Cons"
                                                                  , [Var("q_12"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                  )
                                                                , Op("Nil", [])
                                                                )
                                                              , Var("t_12")
                                                              ]
                                                            )
                                                          , Op("Nil", [])
                                                          )
                                                        ]
                                                      )
                                                    , Op("Nil", [])
                                                    )
                                                  , Anno(Str(">>>"), Op("Nil", []))
                                                  , Var("u_12")
                                                  ]
                                                )
                                              , Op("Nil", [])
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "desugar_arrow_p__0_1"
        , []
        , [VarDec("o_32", ConstType(Sort("ATerm", [])))]
        , GuardedLChoice(
            Scope(
              ["j_17", "k_17", "l_17", "m_17"]
            , Seq(
                Match(
                  Anno(
                    Op("ArrFirst", [Var("k_17"), Var("j_17")])
                  , Wld()
                  )
                )
              , Seq(
                  Match(Var("m_17"))
                , Seq(
                    Build(Var("o_32"))
                  , Seq(
                      CallT(SVar("tuple_pat_0_0"), [], [])
                    , Seq(
                        Match(Var("l_17"))
                      , Seq(
                          Build(Var("m_17"))
                        , Build(
                            Anno(
                              Op(
                                "OpApp"
                              , [ Anno(
                                    Op(
                                      "AppBin"
                                    , [ Anno(
                                          Op(
                                            "Var"
                                          , [Anno(Str("arr"), Op("Nil", []))]
                                          )
                                        , Op("Nil", [])
                                        )
                                      , Anno(
                                          Op(
                                            "Abs"
                                          , [ Anno(
                                                Op(
                                                  "Cons"
                                                , [Var("l_17"), Anno(Op("Nil", []), Op("Nil", []))]
                                                )
                                              , Op("Nil", [])
                                              )
                                            , Var("j_17")
                                            ]
                                          )
                                        , Op("Nil", [])
                                        )
                                      ]
                                    )
                                  , Op("Nil", [])
                                  )
                                , Anno(Str(">>>"), Op("Nil", []))
                                , Var("k_17")
                                ]
                              )
                            , Op("Nil", [])
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          , Id()
          , GuardedLChoice(
              Scope(
                ["e_17", "f_17", "g_17", "h_17"]
              , Seq(
                  Match(
                    Anno(
                      Op("ArrHigher", [Var("e_17"), Var("f_17")])
                    , Wld()
                    )
                  )
                , Seq(
                    Match(Var("h_17"))
                  , Seq(
                      Build(Var("o_32"))
                    , Seq(
                        CallT(SVar("tuple_pat_0_0"), [], [])
                      , Seq(
                          Match(Var("g_17"))
                        , Seq(
                            Build(Var("h_17"))
                          , Build(
                              Anno(
                                Op(
                                  "OpApp"
                                , [ Anno(
                                      Op(
                                        "AppBin"
                                      , [ Anno(
                                            Op(
                                              "Var"
                                            , [Anno(Str("arr"), Op("Nil", []))]
                                            )
                                          , Op("Nil", [])
                                          )
                                        , Anno(
                                            Op(
                                              "Abs"
                                            , [ Anno(
                                                  Op(
                                                    "Cons"
                                                  , [Var("g_17"), Anno(Op("Nil", []), Op("Nil", []))]
                                                  )
                                                , Op("Nil", [])
                                                )
                                              , Anno(
                                                  Op(
                                                    "Product"
                                                  , [ Anno(
                                                        Op(
                                                          "ECons"
                                                        , [ Var("e_17")
                                                          , Anno(
                                                              Op(
                                                                "Cons"
                                                              , [Var("f_17"), Anno(Op("Nil", []), Op("Nil", []))]
                                                              )
                                                            , Op("Nil", [])
                                                            )
                                                          ]
                                                        )
                                                      , Op("Nil", [])
                                                      )
                                                    ]
                                                  )
                                                , Op("Nil", [])
                                                )
                                              ]
                                            )
                                          , Op("Nil", [])
                                          )
                                        ]
                                      )
                                    , Op("Nil", [])
                                    )
                                  , Anno(Str(">>>"), Op("Nil", []))
                                  , Anno(
                                      Op(
                                        "Var"
                                      , [Anno(Str("app"), Op("Nil", []))]
                                      )
                                    , Op("Nil", [])
                                    )
                                  ]
                                )
                              , Op("Nil", [])
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            , Id()
            , GuardedLChoice(
                Scope(
                  [ "q_16"
                  , "r_16"
                  , "s_16"
                  , "t_16"
                  , "y_16"
                  , "u_16"
                  , "z_16"
                  , "v_16"
                  , "a_17"
                  , "w_16"
                  , "b_17"
                  , "x_16"
                  , "c_17"
                  ]
                , Seq(
                    Match(
                      Anno(
                        Op(
                          "ArrIf"
                        , [Var("q_16"), Var("r_16"), Var("s_16")]
                        )
                      , Wld()
                      )
                    )
                  , Seq(
                      Match(Var("y_16"))
                    , Seq(
                        Build(Var("o_32"))
                      , Seq(
                          CallT(SVar("tuple_pat_0_0"), [], [])
                        , Seq(
                            Match(Var("t_16"))
                          , Seq(
                              Build(Var("y_16"))
                            , Seq(
                                Match(Var("z_16"))
                              , Seq(
                                  Build(Var("o_32"))
                                , Seq(
                                    CallT(SVar("tuple_0_0"), [], [])
                                  , Seq(
                                      Match(Var("u_16"))
                                    , Seq(
                                        Build(Var("z_16"))
                                      , Seq(
                                          Match(Var("a_17"))
                                        , Seq(
                                            Build(Var("o_32"))
                                          , Seq(
                                              CallT(SVar("tuple_0_0"), [], [])
                                            , Seq(
                                                Match(Var("v_16"))
                                              , Seq(
                                                  Build(Var("a_17"))
                                                , Seq(
                                                    Match(Var("b_17"))
                                                  , Seq(
                                                      Build(Var("r_16"))
                                                    , Seq(
                                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                                      , Seq(
                                                          Match(Var("w_16"))
                                                        , Seq(
                                                            Build(Var("b_17"))
                                                          , Seq(
                                                              Match(Var("c_17"))
                                                            , Seq(
                                                                Build(Var("s_16"))
                                                              , Seq(
                                                                  CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                                                , Seq(
                                                                    Match(Var("x_16"))
                                                                  , Seq(
                                                                      Build(Var("c_17"))
                                                                    , Build(
                                                                        Anno(
                                                                          Op(
                                                                            "OpApp"
                                                                          , [ Anno(
                                                                                Op(
                                                                                  "AppBin"
                                                                                , [ Anno(
                                                                                      Op(
                                                                                        "Var"
                                                                                      , [Anno(Str("arr"), Op("Nil", []))]
                                                                                      )
                                                                                    , Op("Nil", [])
                                                                                    )
                                                                                  , Anno(
                                                                                      Op(
                                                                                        "Abs"
                                                                                      , [ Anno(
                                                                                            Op(
                                                                                              "Cons"
                                                                                            , [Var("t_16"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                            )
                                                                                          , Op("Nil", [])
                                                                                          )
                                                                                        , Anno(
                                                                                            Op(
                                                                                              "If"
                                                                                            , [ Var("q_16")
                                                                                              , Anno(
                                                                                                  Op(
                                                                                                    "AppBin"
                                                                                                  , [ Anno(
                                                                                                        Op(
                                                                                                          "Constr"
                                                                                                        , [Anno(Str("Left"), Op("Nil", []))]
                                                                                                        )
                                                                                                      , Op("Nil", [])
                                                                                                      )
                                                                                                    , Var("u_16")
                                                                                                    ]
                                                                                                  )
                                                                                                , Op("Nil", [])
                                                                                                )
                                                                                              , Anno(
                                                                                                  Op(
                                                                                                    "AppBin"
                                                                                                  , [ Anno(
                                                                                                        Op(
                                                                                                          "Constr"
                                                                                                        , [Anno(Str("Right"), Op("Nil", []))]
                                                                                                        )
                                                                                                      , Op("Nil", [])
                                                                                                      )
                                                                                                    , Var("v_16")
                                                                                                    ]
                                                                                                  )
                                                                                                , Op("Nil", [])
                                                                                                )
                                                                                              ]
                                                                                            )
                                                                                          , Op("Nil", [])
                                                                                          )
                                                                                        ]
                                                                                      )
                                                                                    , Op("Nil", [])
                                                                                    )
                                                                                  ]
                                                                                )
                                                                              , Op("Nil", [])
                                                                              )
                                                                            , Anno(Str(">>>"), Op("Nil", []))
                                                                            , Anno(
                                                                                Op(
                                                                                  "OpApp"
                                                                                , [ Var("w_16")
                                                                                  , Anno(Str("|||"), Op("Nil", []))
                                                                                  , Var("x_16")
                                                                                  ]
                                                                                )
                                                                              , Op("Nil", [])
                                                                              )
                                                                            ]
                                                                          )
                                                                        , Op("Nil", [])
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              , Id()
              , GuardedLChoice(
                  Scope(
                    [ "e_16"
                    , "f_16"
                    , "g_16"
                    , "h_16"
                    , "i_16"
                    , "j_16"
                    , "m_16"
                    , "k_16"
                    , "n_16"
                    , "l_16"
                    , "o_16"
                    ]
                  , Seq(
                      Match(
                        Anno(
                          Op("ArrLet", [Var("f_16"), Var("e_16")])
                        , Wld()
                        )
                      )
                    , Seq(
                        Match(Var("i_16"))
                      , Seq(
                          Build(Var("f_16"))
                        , Seq(
                            CallT(SVar("free_decls_vars_0_0"), [], [])
                          , Seq(
                              Match(Var("g_16"))
                            , Seq(
                                Build(
                                  Anno(
                                    Op("", [Var("o_32"), Var("g_16")])
                                  , Op("Nil", [])
                                  )
                                )
                              , Seq(
                                  CallT(SVar("conc_0_0"), [], [])
                                , Seq(
                                    Match(Var("h_16"))
                                  , Seq(
                                      Build(Var("i_16"))
                                    , Seq(
                                        Match(Var("m_16"))
                                      , Seq(
                                          Build(Var("o_32"))
                                        , Seq(
                                            CallT(SVar("tuple_pat_0_0"), [], [])
                                          , Seq(
                                              Match(Var("j_16"))
                                            , Seq(
                                                Build(Var("m_16"))
                                              , Seq(
                                                  Match(Var("n_16"))
                                                , Seq(
                                                    Build(Var("h_16"))
                                                  , Seq(
                                                      CallT(SVar("tuple_0_0"), [], [])
                                                    , Seq(
                                                        Match(Var("k_16"))
                                                      , Seq(
                                                          Build(Var("n_16"))
                                                        , Seq(
                                                            Match(Var("o_16"))
                                                          , Seq(
                                                              Build(Var("e_16"))
                                                            , Seq(
                                                                CallT(SVar("desugar_arrow_p__0_1"), [], [Var("h_16")])
                                                              , Seq(
                                                                  Match(Var("l_16"))
                                                                , Seq(
                                                                    Build(Var("o_16"))
                                                                  , Build(
                                                                      Anno(
                                                                        Op(
                                                                          "OpApp"
                                                                        , [ Anno(
                                                                              Op(
                                                                                "AppBin"
                                                                              , [ Anno(
                                                                                    Op(
                                                                                      "Var"
                                                                                    , [Anno(Str("arr"), Op("Nil", []))]
                                                                                    )
                                                                                  , Op("Nil", [])
                                                                                  )
                                                                                , Anno(
                                                                                    Op(
                                                                                      "Abs"
                                                                                    , [ Anno(
                                                                                          Op(
                                                                                            "Cons"
                                                                                          , [Var("j_16"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                          )
                                                                                        , Op("Nil", [])
                                                                                        )
                                                                                      , Anno(
                                                                                          Op("Let", [Var("f_16"), Var("k_16")])
                                                                                        , Op("Nil", [])
                                                                                        )
                                                                                      ]
                                                                                    )
                                                                                  , Op("Nil", [])
                                                                                  )
                                                                                ]
                                                                              )
                                                                            , Op("Nil", [])
                                                                            )
                                                                          , Anno(Str(">>>"), Op("Nil", []))
                                                                          , Var("l_16")
                                                                          ]
                                                                        )
                                                                      , Op("Nil", [])
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                , Id()
                , GuardedLChoice(
                    Scope(
                      [ "s_15"
                      , "t_15"
                      , "u_15"
                      , "v_15"
                      , "w_15"
                      , "x_15"
                      , "a_16"
                      , "y_15"
                      , "b_16"
                      , "z_15"
                      , "c_16"
                      ]
                    , Seq(
                        Match(
                          Anno(
                            Op(
                              "ArrAbs"
                            , [ Anno(
                                  Op(
                                    "Cons"
                                  , [Var("t_15"), Anno(Op("Nil", []), Wld())]
                                  )
                                , Wld()
                                )
                              , Var("s_15")
                              ]
                            )
                          , Wld()
                          )
                        )
                      , Seq(
                          Match(Var("w_15"))
                        , Seq(
                            Build(Var("t_15"))
                          , Seq(
                              CallT(SVar("free_pat_vars_0_0"), [], [])
                            , Seq(
                                Match(Var("u_15"))
                              , Seq(
                                  Build(
                                    Anno(
                                      Op("", [Var("o_32"), Var("u_15")])
                                    , Op("Nil", [])
                                    )
                                  )
                                , Seq(
                                    CallT(SVar("conc_0_0"), [], [])
                                  , Seq(
                                      Match(Var("v_15"))
                                    , Seq(
                                        Build(Var("w_15"))
                                      , Seq(
                                          Match(Var("a_16"))
                                        , Seq(
                                            Build(Var("o_32"))
                                          , Seq(
                                              CallT(SVar("tuple_pat_0_0"), [], [])
                                            , Seq(
                                                Match(Var("x_15"))
                                              , Seq(
                                                  Build(Var("a_16"))
                                                , Seq(
                                                    Match(Var("b_16"))
                                                  , Seq(
                                                      Build(Var("v_15"))
                                                    , Seq(
                                                        CallT(SVar("tuple_0_0"), [], [])
                                                      , Seq(
                                                          Match(Var("y_15"))
                                                        , Seq(
                                                            Build(Var("b_16"))
                                                          , Seq(
                                                              Match(Var("c_16"))
                                                            , Seq(
                                                                Build(Var("s_15"))
                                                              , Seq(
                                                                  CallT(SVar("desugar_arrow_p__0_1"), [], [Var("v_15")])
                                                                , Seq(
                                                                    Match(Var("z_15"))
                                                                  , Seq(
                                                                      Build(Var("c_16"))
                                                                    , Build(
                                                                        Anno(
                                                                          Op(
                                                                            "OpApp"
                                                                          , [ Anno(
                                                                                Op(
                                                                                  "AppBin"
                                                                                , [ Anno(
                                                                                      Op(
                                                                                        "Var"
                                                                                      , [Anno(Str("arr"), Op("Nil", []))]
                                                                                      )
                                                                                    , Op("Nil", [])
                                                                                    )
                                                                                  , Anno(
                                                                                      Op(
                                                                                        "Abs"
                                                                                      , [ Anno(
                                                                                            Op(
                                                                                              "Cons"
                                                                                            , [ Anno(
                                                                                                  Op(
                                                                                                    "TuplePat"
                                                                                                  , [ Var("x_15")
                                                                                                    , Anno(
                                                                                                        Op(
                                                                                                          "Cons"
                                                                                                        , [ Anno(
                                                                                                              Op("Var", [Var("t_15")])
                                                                                                            , Op("Nil", [])
                                                                                                            )
                                                                                                          , Anno(Op("Nil", []), Op("Nil", []))
                                                                                                          ]
                                                                                                        )
                                                                                                      , Op("Nil", [])
                                                                                                      )
                                                                                                    ]
                                                                                                  )
                                                                                                , Op("Nil", [])
                                                                                                )
                                                                                              , Anno(Op("Nil", []), Op("Nil", []))
                                                                                              ]
                                                                                            )
                                                                                          , Op("Nil", [])
                                                                                          )
                                                                                        , Var("y_15")
                                                                                        ]
                                                                                      )
                                                                                    , Op("Nil", [])
                                                                                    )
                                                                                  ]
                                                                                )
                                                                              , Op("Nil", [])
                                                                              )
                                                                            , Anno(Str(">>>"), Op("Nil", []))
                                                                            , Var("z_15")
                                                                            ]
                                                                          )
                                                                        , Op("Nil", [])
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  , Id()
                  , GuardedLChoice(
                      Scope(
                        ["j_15", "k_15", "l_15", "o_15", "m_15", "p_15", "n_15", "q_15"]
                      , Seq(
                          Match(
                            Anno(
                              Op("ArrAppBin", [Var("k_15"), Var("j_15")])
                            , Wld()
                            )
                          )
                        , Seq(
                            Match(Var("o_15"))
                          , Seq(
                              Build(Var("o_32"))
                            , Seq(
                                CallT(SVar("tuple_pat_0_0"), [], [])
                              , Seq(
                                  Match(Var("l_15"))
                                , Seq(
                                    Build(Var("o_15"))
                                  , Seq(
                                      Match(Var("p_15"))
                                    , Seq(
                                        Build(Var("o_32"))
                                      , Seq(
                                          CallT(SVar("tuple_0_0"), [], [])
                                        , Seq(
                                            Match(Var("m_15"))
                                          , Seq(
                                              Build(Var("p_15"))
                                            , Seq(
                                                Match(Var("q_15"))
                                              , Seq(
                                                  Build(Var("k_15"))
                                                , Seq(
                                                    CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                                  , Seq(
                                                      Match(Var("n_15"))
                                                    , Seq(
                                                        Build(Var("q_15"))
                                                      , Build(
                                                          Anno(
                                                            Op(
                                                              "OpApp"
                                                            , [ Anno(
                                                                  Op(
                                                                    "AppBin"
                                                                  , [ Anno(
                                                                        Op(
                                                                          "Var"
                                                                        , [Anno(Str("arr"), Op("Nil", []))]
                                                                        )
                                                                      , Op("Nil", [])
                                                                      )
                                                                    , Anno(
                                                                        Op(
                                                                          "Abs"
                                                                        , [ Anno(
                                                                              Op(
                                                                                "Cons"
                                                                              , [Var("l_15"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                              )
                                                                            , Op("Nil", [])
                                                                            )
                                                                          , Anno(
                                                                              Op(
                                                                                "Product"
                                                                              , [ Anno(
                                                                                    Op(
                                                                                      "ECons"
                                                                                    , [ Var("m_15")
                                                                                      , Anno(
                                                                                          Op(
                                                                                            "Cons"
                                                                                          , [Var("j_15"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                          )
                                                                                        , Op("Nil", [])
                                                                                        )
                                                                                      ]
                                                                                    )
                                                                                  , Op("Nil", [])
                                                                                  )
                                                                                ]
                                                                              )
                                                                            , Op("Nil", [])
                                                                            )
                                                                          ]
                                                                        )
                                                                      , Op("Nil", [])
                                                                      )
                                                                    ]
                                                                  )
                                                                , Op("Nil", [])
                                                                )
                                                              , Anno(Str(">>>"), Op("Nil", []))
                                                              , Var("n_15")
                                                              ]
                                                            )
                                                          , Op("Nil", [])
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    , Id()
                    , GuardedLChoice(
                        Scope(
                          [ "y_14"
                          , "z_14"
                          , "a_15"
                          , "b_15"
                          , "c_15"
                          , "e_15"
                          , "d_15"
                          , "f_15"
                          , "g_15"
                          , "h_15"
                          ]
                        , Seq(
                            Match(
                              Anno(
                                Op("ArrForm", [Var("y_14"), Var("z_14")])
                              , Wld()
                              )
                            )
                          , Seq(
                              Match(Var("b_15"))
                            , Seq(
                                Match(Var("e_15"))
                              , Seq(
                                  Build(Var("o_32"))
                                , Seq(
                                    CallT(SVar("tuple_pat_0_0"), [], [])
                                  , Seq(
                                      Match(Var("c_15"))
                                    , Seq(
                                        Build(Var("e_15"))
                                      , Seq(
                                          Match(Var("f_15"))
                                        , Seq(
                                            Build(Var("o_32"))
                                          , Seq(
                                              CallT(SVar("tuple_0_0"), [], [])
                                            , Seq(
                                                Match(Var("d_15"))
                                              , Seq(
                                                  Build(Var("f_15"))
                                                , Seq(
                                                    Build(
                                                      Anno(
                                                        Op(
                                                          "Abs"
                                                        , [ Anno(
                                                              Op(
                                                                "Cons"
                                                              , [Var("c_15"), Anno(Op("Nil", []), Op("Nil", []))]
                                                              )
                                                            , Op("Nil", [])
                                                            )
                                                          , Var("d_15")
                                                          ]
                                                        )
                                                      , Op("Nil", [])
                                                      )
                                                    )
                                                  , Seq(
                                                      Match(Var("a_15"))
                                                    , Seq(
                                                        Build(Var("b_15"))
                                                      , Seq(
                                                          Match(Var("h_15"))
                                                        , Seq(
                                                            Build(Var("z_14"))
                                                          , Seq(
                                                              CallT(
                                                                SVar("map_1_0")
                                                              , [CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])]
                                                              , []
                                                              )
                                                            , Seq(
                                                                Match(Var("g_15"))
                                                              , Seq(
                                                                  Build(Var("h_15"))
                                                                , Seq(
                                                                    Build(
                                                                      Anno(
                                                                        Op("", [Var("y_14"), Var("g_15")])
                                                                      , Op("Nil", [])
                                                                      )
                                                                    )
                                                                  , CallT(SVar("apply_all_0_1"), [], [Var("a_15")])
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      , Id()
                      , GuardedLChoice(
                          Scope(
                            ["u_14", "v_14", "w_14"]
                          , Seq(
                              Match(
                                Anno(
                                  Op(
                                    "ArrOpApp"
                                  , [Var("v_14"), Var("u_14"), Var("w_14")]
                                  )
                                , Wld()
                                )
                              )
                            , Seq(
                                Build(
                                  Anno(
                                    Op(
                                      "ArrForm"
                                    , [ Anno(
                                          Op("BinCon", [Var("u_14")])
                                        , Op("Nil", [])
                                        )
                                      , Anno(
                                          Op(
                                            "Cons"
                                          , [ Var("v_14")
                                            , Anno(
                                                Op(
                                                  "Cons"
                                                , [Var("w_14"), Anno(Op("Nil", []), Op("Nil", []))]
                                                )
                                              , Op("Nil", [])
                                              )
                                            ]
                                          )
                                        , Op("Nil", [])
                                        )
                                      ]
                                    )
                                  , Op("Nil", [])
                                  )
                                )
                              , CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                              )
                            )
                          )
                        , Id()
                        , GuardedLChoice(
                            Scope(
                              ["s_14"]
                            , Seq(
                                Match(
                                  Anno(
                                    Op(
                                      "ArrDo"
                                    , [ Anno(
                                          Op(
                                            "ArrStmtList"
                                          , [Anno(Op("ArrCmdStmt", [Var("s_14")]), Wld())]
                                          )
                                        , Wld()
                                        )
                                      ]
                                    )
                                  , Wld()
                                  )
                                )
                              , Seq(
                                  Build(Var("s_14"))
                                , CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                )
                              )
                            )
                          , Id()
                          , GuardedLChoice(
                              Scope(
                                [ "g_14"
                                , "h_14"
                                , "i_14"
                                , "j_14"
                                , "k_14"
                                , "l_14"
                                , "o_14"
                                , "m_14"
                                , "p_14"
                                , "n_14"
                                , "q_14"
                                ]
                              , Seq(
                                  Match(
                                    Anno(
                                      Op(
                                        "ArrDo"
                                      , [ Anno(
                                            Op(
                                              "ArrStmtList"
                                            , [ Anno(
                                                  Op(
                                                    "ArrStmtSeq"
                                                  , [Anno(Op("ArrLetStmt", [Var("h_14")]), Wld()), Var("g_14")]
                                                  )
                                                , Wld()
                                                )
                                              ]
                                            )
                                          , Wld()
                                          )
                                        ]
                                      )
                                    , Wld()
                                    )
                                  )
                                , Seq(
                                    Match(Var("k_14"))
                                  , Seq(
                                      Build(Var("h_14"))
                                    , Seq(
                                        CallT(SVar("free_decls_vars_0_0"), [], [])
                                      , Seq(
                                          Match(Var("i_14"))
                                        , Seq(
                                            Build(
                                              Anno(
                                                Op("", [Var("o_32"), Var("i_14")])
                                              , Op("Nil", [])
                                              )
                                            )
                                          , Seq(
                                              CallT(SVar("conc_0_0"), [], [])
                                            , Seq(
                                                Match(Var("j_14"))
                                              , Seq(
                                                  Build(Var("k_14"))
                                                , Seq(
                                                    Match(Var("o_14"))
                                                  , Seq(
                                                      Build(Var("o_32"))
                                                    , Seq(
                                                        CallT(SVar("tuple_pat_0_0"), [], [])
                                                      , Seq(
                                                          Match(Var("l_14"))
                                                        , Seq(
                                                            Build(Var("o_14"))
                                                          , Seq(
                                                              Match(Var("p_14"))
                                                            , Seq(
                                                                Build(Var("j_14"))
                                                              , Seq(
                                                                  CallT(SVar("tuple_0_0"), [], [])
                                                                , Seq(
                                                                    Match(Var("m_14"))
                                                                  , Seq(
                                                                      Build(Var("p_14"))
                                                                    , Seq(
                                                                        Match(Var("q_14"))
                                                                      , Seq(
                                                                          Build(
                                                                            Anno(
                                                                              Op(
                                                                                "ArrDo"
                                                                              , [Anno(
                                                                                   Op("ArrStmtList", [Var("g_14")])
                                                                                 , Op("Nil", [])
                                                                                 )]
                                                                              )
                                                                            , Op("Nil", [])
                                                                            )
                                                                          )
                                                                        , Seq(
                                                                            CallT(SVar("desugar_arrow_p__0_1"), [], [Var("j_14")])
                                                                          , Seq(
                                                                              Match(Var("n_14"))
                                                                            , Seq(
                                                                                Build(Var("q_14"))
                                                                              , Build(
                                                                                  Anno(
                                                                                    Op(
                                                                                      "OpApp"
                                                                                    , [ Anno(
                                                                                          Op(
                                                                                            "AppBin"
                                                                                          , [ Anno(
                                                                                                Op(
                                                                                                  "Var"
                                                                                                , [Anno(Str("arr"), Op("Nil", []))]
                                                                                                )
                                                                                              , Op("Nil", [])
                                                                                              )
                                                                                            , Anno(
                                                                                                Op(
                                                                                                  "Abs"
                                                                                                , [ Anno(
                                                                                                      Op(
                                                                                                        "Cons"
                                                                                                      , [Var("l_14"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                      )
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  , Anno(
                                                                                                      Op("Let", [Var("h_14"), Var("m_14")])
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  ]
                                                                                                )
                                                                                              , Op("Nil", [])
                                                                                              )
                                                                                            ]
                                                                                          )
                                                                                        , Op("Nil", [])
                                                                                        )
                                                                                      , Anno(Str(">>>"), Op("Nil", []))
                                                                                      , Var("n_14")
                                                                                      ]
                                                                                    )
                                                                                  , Op("Nil", [])
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            , Id()
                            , GuardedLChoice(
                                Scope(
                                  [ "t_13"
                                  , "u_13"
                                  , "v_13"
                                  , "a_14"
                                  , "w_13"
                                  , "b_14"
                                  , "x_13"
                                  , "c_14"
                                  , "y_13"
                                  , "d_14"
                                  , "z_13"
                                  , "e_14"
                                  ]
                                , Seq(
                                    Match(
                                      Anno(
                                        Op(
                                          "ArrDo"
                                        , [ Anno(
                                              Op(
                                                "ArrStmtList"
                                              , [ Anno(
                                                    Op(
                                                      "ArrStmtSeq"
                                                    , [Anno(Op("ArrCmdStmt", [Var("t_13")]), Wld()), Var("u_13")]
                                                    )
                                                  , Wld()
                                                  )
                                                ]
                                              )
                                            , Wld()
                                            )
                                          ]
                                        )
                                      , Wld()
                                      )
                                    )
                                  , Seq(
                                      Match(Var("a_14"))
                                    , Seq(
                                        Build(Var("o_32"))
                                      , Seq(
                                          CallT(SVar("tuple_pat_0_0"), [], [])
                                        , Seq(
                                            Match(Var("v_13"))
                                          , Seq(
                                              Build(Var("a_14"))
                                            , Seq(
                                                Match(Var("b_14"))
                                              , Seq(
                                                  Build(Var("o_32"))
                                                , Seq(
                                                    CallT(SVar("tuple_0_0"), [], [])
                                                  , Seq(
                                                      Match(Var("w_13"))
                                                    , Seq(
                                                        Build(Var("b_14"))
                                                      , Seq(
                                                          Match(Var("c_14"))
                                                        , Seq(
                                                            Build(Var("o_32"))
                                                          , Seq(
                                                              CallT(SVar("tuple_0_0"), [], [])
                                                            , Seq(
                                                                Match(Var("x_13"))
                                                              , Seq(
                                                                  Build(Var("c_14"))
                                                                , Seq(
                                                                    Match(Var("d_14"))
                                                                  , Seq(
                                                                      Build(Var("t_13"))
                                                                    , Seq(
                                                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                                                      , Seq(
                                                                          Match(Var("y_13"))
                                                                        , Seq(
                                                                            Build(Var("d_14"))
                                                                          , Seq(
                                                                              Match(Var("e_14"))
                                                                            , Seq(
                                                                                Build(
                                                                                  Anno(
                                                                                    Op(
                                                                                      "ArrDo"
                                                                                    , [Anno(
                                                                                         Op("ArrStmtList", [Var("u_13")])
                                                                                       , Op("Nil", [])
                                                                                       )]
                                                                                    )
                                                                                  , Op("Nil", [])
                                                                                  )
                                                                                )
                                                                              , Seq(
                                                                                  CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                                                                , Seq(
                                                                                    Match(Var("z_13"))
                                                                                  , Seq(
                                                                                      Build(Var("e_14"))
                                                                                    , Build(
                                                                                        Anno(
                                                                                          Op(
                                                                                            "OpApp"
                                                                                          , [ Anno(
                                                                                                Op(
                                                                                                  "AppBin"
                                                                                                , [ Anno(
                                                                                                      Op(
                                                                                                        "Var"
                                                                                                      , [Anno(Str("arr"), Op("Nil", []))]
                                                                                                      )
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  , Anno(
                                                                                                      Op(
                                                                                                        "Abs"
                                                                                                      , [ Anno(
                                                                                                            Op(
                                                                                                              "Cons"
                                                                                                            , [Var("v_13"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                            )
                                                                                                          , Op("Nil", [])
                                                                                                          )
                                                                                                        , Anno(
                                                                                                            Op(
                                                                                                              "Product"
                                                                                                            , [ Anno(
                                                                                                                  Op(
                                                                                                                    "ECons"
                                                                                                                  , [ Var("w_13")
                                                                                                                    , Anno(
                                                                                                                        Op(
                                                                                                                          "Cons"
                                                                                                                        , [Var("x_13"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                                        )
                                                                                                                      , Op("Nil", [])
                                                                                                                      )
                                                                                                                    ]
                                                                                                                  )
                                                                                                                , Op("Nil", [])
                                                                                                                )
                                                                                                              ]
                                                                                                            )
                                                                                                          , Op("Nil", [])
                                                                                                          )
                                                                                                        ]
                                                                                                      )
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  ]
                                                                                                )
                                                                                              , Op("Nil", [])
                                                                                              )
                                                                                            , Anno(Str(">>>"), Op("Nil", []))
                                                                                            , Anno(
                                                                                                Op(
                                                                                                  "OpApp"
                                                                                                , [ Anno(
                                                                                                      Op(
                                                                                                        "AppBin"
                                                                                                      , [ Anno(
                                                                                                            Op(
                                                                                                              "Var"
                                                                                                            , [Anno(Str("first"), Op("Nil", []))]
                                                                                                            )
                                                                                                          , Op("Nil", [])
                                                                                                          )
                                                                                                        , Var("y_13")
                                                                                                        ]
                                                                                                      )
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  , Anno(Str(">>>"), Op("Nil", []))
                                                                                                  , Anno(
                                                                                                      Op(
                                                                                                        "OpApp"
                                                                                                      , [ Anno(
                                                                                                            Op(
                                                                                                              "AppBin"
                                                                                                            , [ Anno(
                                                                                                                  Op(
                                                                                                                    "Var"
                                                                                                                  , [Anno(Str("arr"), Op("Nil", []))]
                                                                                                                  )
                                                                                                                , Op("Nil", [])
                                                                                                                )
                                                                                                              , Anno(
                                                                                                                  Op(
                                                                                                                    "Var"
                                                                                                                  , [Anno(Str("snd"), Op("Nil", []))]
                                                                                                                  )
                                                                                                                , Op("Nil", [])
                                                                                                                )
                                                                                                              ]
                                                                                                            )
                                                                                                          , Op("Nil", [])
                                                                                                          )
                                                                                                        , Anno(Str(">>>"), Op("Nil", []))
                                                                                                        , Var("z_13")
                                                                                                        ]
                                                                                                      )
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  ]
                                                                                                )
                                                                                              , Op("Nil", [])
                                                                                              )
                                                                                            ]
                                                                                          )
                                                                                        , Op("Nil", [])
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              , Id()
                              , Scope(
                                  [ "y_12"
                                  , "z_12"
                                  , "a_13"
                                  , "b_13"
                                  , "c_13"
                                  , "d_13"
                                  , "e_13"
                                  , "l_13"
                                  , "f_13"
                                  , "m_13"
                                  , "g_13"
                                  , "n_13"
                                  , "h_13"
                                  , "o_13"
                                  , "i_13"
                                  , "p_13"
                                  , "j_13"
                                  , "q_13"
                                  , "k_13"
                                  , "r_13"
                                  ]
                                , Seq(
                                    Match(
                                      Anno(
                                        Op(
                                          "ArrDo"
                                        , [ Anno(
                                              Op(
                                                "ArrStmtList"
                                              , [ Anno(
                                                    Op(
                                                      "ArrStmtSeq"
                                                    , [ Anno(
                                                          Op("ArrBindStmt", [Var("a_13"), Var("y_12")])
                                                        , Wld()
                                                        )
                                                      , Var("z_12")
                                                      ]
                                                    )
                                                  , Wld()
                                                  )
                                                ]
                                              )
                                            , Wld()
                                            )
                                          ]
                                        )
                                      , Wld()
                                      )
                                    )
                                  , Seq(
                                      Match(Var("d_13"))
                                    , Seq(
                                        Build(Var("a_13"))
                                      , Seq(
                                          CallT(SVar("free_pat_vars_0_0"), [], [])
                                        , Seq(
                                            Match(Var("b_13"))
                                          , Seq(
                                              Build(
                                                Anno(
                                                  Op("", [Var("b_13"), Var("o_32")])
                                                , Op("Nil", [])
                                                )
                                              )
                                            , Seq(
                                                CallT(SVar("conc_0_0"), [], [])
                                              , Seq(
                                                  Match(Var("c_13"))
                                                , Seq(
                                                    Build(Var("d_13"))
                                                  , Seq(
                                                      Match(Var("l_13"))
                                                    , Seq(
                                                        Build(Var("o_32"))
                                                      , Seq(
                                                          CallT(SVar("tuple_pat_0_0"), [], [])
                                                        , Seq(
                                                            Match(Var("e_13"))
                                                          , Seq(
                                                              Build(Var("l_13"))
                                                            , Seq(
                                                                Match(Var("m_13"))
                                                              , Seq(
                                                                  Build(Var("o_32"))
                                                                , Seq(
                                                                    CallT(SVar("tuple_0_0"), [], [])
                                                                  , Seq(
                                                                      Match(Var("f_13"))
                                                                    , Seq(
                                                                        Build(Var("m_13"))
                                                                      , Seq(
                                                                          Match(Var("n_13"))
                                                                        , Seq(
                                                                            Build(Var("o_32"))
                                                                          , Seq(
                                                                              CallT(SVar("tuple_0_0"), [], [])
                                                                            , Seq(
                                                                                Match(Var("g_13"))
                                                                              , Seq(
                                                                                  Build(Var("n_13"))
                                                                                , Seq(
                                                                                    Match(Var("o_13"))
                                                                                  , Seq(
                                                                                      Build(Var("y_12"))
                                                                                    , Seq(
                                                                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("o_32")])
                                                                                      , Seq(
                                                                                          Match(Var("h_13"))
                                                                                        , Seq(
                                                                                            Build(Var("o_13"))
                                                                                          , Seq(
                                                                                              Match(Var("p_13"))
                                                                                            , Seq(
                                                                                                Build(Var("o_32"))
                                                                                              , Seq(
                                                                                                  CallT(SVar("tuple_pat_0_0"), [], [])
                                                                                                , Seq(
                                                                                                    Match(Var("i_13"))
                                                                                                  , Seq(
                                                                                                      Build(Var("p_13"))
                                                                                                    , Seq(
                                                                                                        Match(Var("q_13"))
                                                                                                      , Seq(
                                                                                                          Build(Var("c_13"))
                                                                                                        , Seq(
                                                                                                            CallT(SVar("tuple_0_0"), [], [])
                                                                                                          , Seq(
                                                                                                              Match(Var("j_13"))
                                                                                                            , Seq(
                                                                                                                Build(Var("q_13"))
                                                                                                              , Seq(
                                                                                                                  Match(Var("r_13"))
                                                                                                                , Seq(
                                                                                                                    Build(
                                                                                                                      Anno(
                                                                                                                        Op(
                                                                                                                          "ArrDo"
                                                                                                                        , [Anno(
                                                                                                                             Op("ArrStmtList", [Var("z_12")])
                                                                                                                           , Op("Nil", [])
                                                                                                                           )]
                                                                                                                        )
                                                                                                                      , Op("Nil", [])
                                                                                                                      )
                                                                                                                    )
                                                                                                                  , Seq(
                                                                                                                      CallT(SVar("desugar_arrow_p__0_1"), [], [Var("c_13")])
                                                                                                                    , Seq(
                                                                                                                        Match(Var("k_13"))
                                                                                                                      , Seq(
                                                                                                                          Build(Var("r_13"))
                                                                                                                        , Build(
                                                                                                                            Anno(
                                                                                                                              Op(
                                                                                                                                "OpApp"
                                                                                                                              , [ Anno(
                                                                                                                                    Op(
                                                                                                                                      "AppBin"
                                                                                                                                    , [ Anno(
                                                                                                                                          Op(
                                                                                                                                            "Var"
                                                                                                                                          , [Anno(Str("arr"), Op("Nil", []))]
                                                                                                                                          )
                                                                                                                                        , Op("Nil", [])
                                                                                                                                        )
                                                                                                                                      , Anno(
                                                                                                                                          Op(
                                                                                                                                            "Abs"
                                                                                                                                          , [ Anno(
                                                                                                                                                Op(
                                                                                                                                                  "Cons"
                                                                                                                                                , [Var("e_13"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                                                                )
                                                                                                                                              , Op("Nil", [])
                                                                                                                                              )
                                                                                                                                            , Anno(
                                                                                                                                                Op(
                                                                                                                                                  "Product"
                                                                                                                                                , [ Anno(
                                                                                                                                                      Op(
                                                                                                                                                        "ECons"
                                                                                                                                                      , [ Var("f_13")
                                                                                                                                                        , Anno(
                                                                                                                                                            Op(
                                                                                                                                                              "Cons"
                                                                                                                                                            , [Var("g_13"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                                                                            )
                                                                                                                                                          , Op("Nil", [])
                                                                                                                                                          )
                                                                                                                                                        ]
                                                                                                                                                      )
                                                                                                                                                    , Op("Nil", [])
                                                                                                                                                    )
                                                                                                                                                  ]
                                                                                                                                                )
                                                                                                                                              , Op("Nil", [])
                                                                                                                                              )
                                                                                                                                            ]
                                                                                                                                          )
                                                                                                                                        , Op("Nil", [])
                                                                                                                                        )
                                                                                                                                      ]
                                                                                                                                    )
                                                                                                                                  , Op("Nil", [])
                                                                                                                                  )
                                                                                                                                , Anno(Str(">>>"), Op("Nil", []))
                                                                                                                                , Anno(
                                                                                                                                    Op(
                                                                                                                                      "OpApp"
                                                                                                                                    , [ Anno(
                                                                                                                                          Op(
                                                                                                                                            "AppBin"
                                                                                                                                          , [ Anno(
                                                                                                                                                Op(
                                                                                                                                                  "Var"
                                                                                                                                                , [Anno(Str("first"), Op("Nil", []))]
                                                                                                                                                )
                                                                                                                                              , Op("Nil", [])
                                                                                                                                              )
                                                                                                                                            , Var("h_13")
                                                                                                                                            ]
                                                                                                                                          )
                                                                                                                                        , Op("Nil", [])
                                                                                                                                        )
                                                                                                                                      , Anno(Str(">>>"), Op("Nil", []))
                                                                                                                                      , Anno(
                                                                                                                                          Op(
                                                                                                                                            "OpApp"
                                                                                                                                          , [ Anno(
                                                                                                                                                Op(
                                                                                                                                                  "AppBin"
                                                                                                                                                , [ Anno(
                                                                                                                                                      Op(
                                                                                                                                                        "Var"
                                                                                                                                                      , [Anno(Str("arr"), Op("Nil", []))]
                                                                                                                                                      )
                                                                                                                                                    , Op("Nil", [])
                                                                                                                                                    )
                                                                                                                                                  , Anno(
                                                                                                                                                      Op(
                                                                                                                                                        "Abs"
                                                                                                                                                      , [ Anno(
                                                                                                                                                            Op(
                                                                                                                                                              "Cons"
                                                                                                                                                            , [ Anno(
                                                                                                                                                                  Op(
                                                                                                                                                                    "TuplePat"
                                                                                                                                                                  , [ Var("a_13")
                                                                                                                                                                    , Anno(
                                                                                                                                                                        Op(
                                                                                                                                                                          "Cons"
                                                                                                                                                                        , [Var("i_13"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                                                                                        )
                                                                                                                                                                      , Op("Nil", [])
                                                                                                                                                                      )
                                                                                                                                                                    ]
                                                                                                                                                                  )
                                                                                                                                                                , Op("Nil", [])
                                                                                                                                                                )
                                                                                                                                                              , Anno(Op("Nil", []), Op("Nil", []))
                                                                                                                                                              ]
                                                                                                                                                            )
                                                                                                                                                          , Op("Nil", [])
                                                                                                                                                          )
                                                                                                                                                        , Var("j_13")
                                                                                                                                                        ]
                                                                                                                                                      )
                                                                                                                                                    , Op("Nil", [])
                                                                                                                                                    )
                                                                                                                                                  ]
                                                                                                                                                )
                                                                                                                                              , Op("Nil", [])
                                                                                                                                              )
                                                                                                                                            , Anno(Str(">>>"), Op("Nil", []))
                                                                                                                                            , Var("k_13")
                                                                                                                                            ]
                                                                                                                                          )
                                                                                                                                        , Op("Nil", [])
                                                                                                                                        )
                                                                                                                                      ]
                                                                                                                                    )
                                                                                                                                  , Op("Nil", [])
                                                                                                                                  )
                                                                                                                                ]
                                                                                                                              )
                                                                                                                            , Op("Nil", [])
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "tuple_pat_0_0"
        , []
        , []
        , GuardedLChoice(
            Seq(
              Match(Anno(Op("Nil", []), Wld()))
            , Build(
                Anno(
                  Op(
                    "ConstrPat"
                  , [Anno(Op("Unit", []), Op("Nil", []))]
                  )
                , Op("Nil", [])
                )
              )
            )
          , Id()
          , GuardedLChoice(
              Scope(
                ["p_17"]
              , Seq(
                  Match(
                    Anno(
                      Op(
                        "Cons"
                      , [Var("p_17"), Anno(Op("Nil", []), Wld())]
                      )
                    , Wld()
                    )
                  )
                , Build(Var("p_17"))
                )
              )
            , Id()
            , Scope(
                ["n_17", "o_17"]
              , Seq(
                  Match(
                    Anno(
                      Op("Cons", [Var("n_17"), Var("o_17")])
                    , Wld()
                    )
                  )
                , Build(
                    Anno(
                      Op("TuplePat", [Var("n_17"), Var("o_17")])
                    , Op("Nil", [])
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "tuple_0_0"
        , []
        , []
        , GuardedLChoice(
            Seq(
              Match(Anno(Op("Nil", []), Wld()))
            , Build(
                Anno(
                  Op(
                    "Constr"
                  , [Anno(Op("Unit", []), Op("Nil", []))]
                  )
                , Op("Nil", [])
                )
              )
            )
          , Id()
          , GuardedLChoice(
              Scope(
                ["s_17"]
              , Seq(
                  Match(
                    Anno(
                      Op(
                        "Cons"
                      , [Var("s_17"), Anno(Op("Nil", []), Wld())]
                      )
                    , Wld()
                    )
                  )
                , Build(Var("s_17"))
                )
              )
            , Id()
            , Scope(
                ["q_17", "r_17"]
              , Seq(
                  Match(
                    Anno(
                      Op("Cons", [Var("q_17"), Var("r_17")])
                    , Wld()
                    )
                  )
                , Build(
                    Anno(
                      Op(
                        "Product"
                      , [ Anno(
                            Op("ECons", [Var("q_17"), Var("r_17")])
                          , Op("Nil", [])
                          )
                        ]
                      )
                    , Op("Nil", [])
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "free_pat_vars_0_0"
        , []
        , []
        , CallT(
            SVar("collect_all_1_0")
          , [Match(Anno(Op("Var", [Wld()]), Wld()))]
          , []
          )
        )
      , SDefT(
          "free_decls_vars_0_0"
        , []
        , []
        , CallT(
            SVar("collect_all_3_0")
          , [ Match(Anno(Op("Var", [Wld()]), Wld()))
            , CallT(SVar("union_0_0"), [], [])
            , Scope(
                ["t_17"]
              , Seq(
                  Match(
                    Anno(Op("VarFunLHS", [Var("t_17"), Wld()]), Wld())
                  )
                , Build(Var("t_17"))
                )
              )
            ]
          , []
          )
        )
      , SDefT(
          "apply_all_0_1"
        , []
        , [VarDec("p_32", ConstType(Sort("ATerm", [])))]
        , GuardedLChoice(
            Scope(
              ["z_17"]
            , Seq(
                Match(
                  Anno(
                    Op(
                      ""
                    , [Var("z_17"), Anno(Op("Nil", []), Wld())]
                    )
                  , Wld()
                  )
                )
              , Build(Var("z_17"))
              )
            )
          , Id()
          , Scope(
              ["v_17", "w_17", "x_17"]
            , Seq(
                Match(
                  Anno(
                    Op(
                      ""
                    , [ Var("v_17")
                      , Anno(
                          Op("Cons", [Var("w_17"), Var("x_17")])
                        , Wld()
                        )
                      ]
                    )
                  , Wld()
                  )
                )
              , Seq(
                  Build(
                    Anno(
                      Op(
                        ""
                      , [ Anno(
                            Op(
                              "AppBin"
                            , [ Var("v_17")
                              , Anno(
                                  Op(
                                    "OpApp"
                                  , [ Anno(
                                        Op(
                                          "AppBin"
                                        , [ Anno(
                                              Op(
                                                "Var"
                                              , [Anno(Str("arr"), Op("Nil", []))]
                                              )
                                            , Op("Nil", [])
                                            )
                                          , Var("p_32")
                                          ]
                                        )
                                      , Op("Nil", [])
                                      )
                                    , Anno(Str(">>>"), Op("Nil", []))
                                    , Var("w_17")
                                    ]
                                  )
                                , Op("Nil", [])
                                )
                              ]
                            )
                          , Op("Nil", [])
                          )
                        , Var("x_17")
                        ]
                      )
                    , Op("Nil", [])
                    )
                  )
                , CallT(SVar("apply_all_0_1"), [], [Var("p_32")])
                )
              )
            )
          )
        )
      , SDefT(
          "map_1_0"
        , [ VarDec(
              "f_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "g_18"
              , []
              , []
              , GuardedLChoice(
                  Match(Anno(Op("Nil", []), Wld()))
                , Id()
                , Scope(
                    ["a_18", "b_18", "c_18", "d_18", "e_18"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("a_18"), Var("b_18")])
                        , Var("e_18")
                        )
                      )
                    , Seq(
                        Build(Var("a_18"))
                      , Seq(
                          CallT(SVar("f_18"), [], [])
                        , Seq(
                            Match(Var("c_18"))
                          , Seq(
                              Build(Var("b_18"))
                            , Seq(
                                CallT(SVar("g_18"), [], [])
                              , Seq(
                                  Match(Var("d_18"))
                                , Build(
                                    Anno(
                                      Op("Cons", [Var("c_18"), Var("d_18")])
                                    , Var("e_18")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ]
          , CallT(SVar("g_18"), [], [])
          )
        )
      , SDefT(
          "collect_all_1_0"
        , [ VarDec(
              "h_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , CallT(
            SVar("collect_all_2_0")
          , [ CallT(SVar("h_18"), [], [])
            , CallT(SVar("union_0_0"), [], [])
            ]
          , []
          )
        )
      , SDefT(
          "collect_all_2_0"
        , [ VarDec(
              "i_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "k_18"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["l_18", "n_18", "m_18", "o_18"]
                  , Seq(
                      Match(Var("n_18"))
                    , Seq(
                        CallT(SVar("i_18"), [], [])
                      , Seq(
                          Match(Var("l_18"))
                        , Seq(
                            Build(Var("n_18"))
                          , Seq(
                              Match(Var("o_18"))
                            , Seq(
                                CallT(
                                  SVar("crush_3_0")
                                , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                                  , CallT(SVar("j_18"), [], [])
                                  , CallT(SVar("k_18"), [], [])
                                  ]
                                , []
                                )
                              , Seq(
                                  Match(Var("m_18"))
                                , Seq(
                                    Build(Var("o_18"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("l_18"), Var("m_18")])
                                      , Op("Nil", [])
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                , Id()
                , CallT(
                    SVar("crush_3_0")
                  , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                    , CallT(SVar("j_18"), [], [])
                    , CallT(SVar("k_18"), [], [])
                    ]
                  , []
                  )
                )
              )
            ]
          , CallT(SVar("k_18"), [], [])
          )
        )
      , SDefT(
          "collect_all_3_0"
        , [ VarDec(
              "p_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "s_18"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["t_18", "v_18", "u_18", "w_18"]
                  , Seq(
                      Match(Var("v_18"))
                    , Seq(
                        CallT(SVar("p_18"), [], [])
                      , Seq(
                          Match(Var("t_18"))
                        , Seq(
                            Build(Var("v_18"))
                          , Seq(
                              Match(Var("w_18"))
                            , Seq(
                                CallT(
                                  SVar("crush_3_0")
                                , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                                  , CallT(SVar("q_18"), [], [])
                                  , CallT(SVar("s_18"), [], [])
                                  ]
                                , []
                                )
                              , Seq(
                                  Match(Var("u_18"))
                                , Seq(
                                    Build(Var("w_18"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("t_18"), Var("u_18")])
                                      , Op("Nil", [])
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                , Id()
                , GuardedLChoice(
                    Seq(
                      CallT(SVar("r_18"), [], [])
                    , CallT(SVar("s_18"), [], [])
                    )
                  , Id()
                  , CallT(
                      SVar("crush_3_0")
                    , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                      , CallT(SVar("q_18"), [], [])
                      , CallT(SVar("s_18"), [], [])
                      ]
                    , []
                    )
                  )
                )
              )
            ]
          , CallT(SVar("s_18"), [], [])
          )
        )
      , SDefT(
          "crush_3_0"
        , [ VarDec(
              "y_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "a_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_18"]
          , Seq(
              Match(Anno(Explode(Wld(), Var("x_18")), Wld()))
            , Seq(
                Build(Var("x_18"))
              , CallT(
                  SVar("foldr_3_0")
                , [ CallT(SVar("y_18"), [], [])
                  , CallT(SVar("z_18"), [], [])
                  , CallT(SVar("a_19"), [], [])
                  ]
                , []
                )
              )
            )
          )
        )
      , SDefT(
          "foldr_3_0"
        , [ VarDec(
              "d_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , GuardedLChoice(
            Seq(
              Match(Anno(Op("Nil", []), Wld()))
            , CallT(SVar("d_19"), [], [])
            )
          , Id()
          , Scope(
              ["b_19", "c_19", "g_19", "i_19", "h_19", "j_19"]
            , Seq(
                Match(
                  Anno(
                    Op("Cons", [Var("b_19"), Var("c_19")])
                  , Wld()
                  )
                )
              , Seq(
                  Match(Var("i_19"))
                , Seq(
                    Build(Var("b_19"))
                  , Seq(
                      CallT(SVar("f_19"), [], [])
                    , Seq(
                        Match(Var("g_19"))
                      , Seq(
                          Build(Var("i_19"))
                        , Seq(
                            Match(Var("j_19"))
                          , Seq(
                              Build(Var("c_19"))
                            , Seq(
                                CallT(
                                  SVar("foldr_3_0")
                                , [ CallT(SVar("d_19"), [], [])
                                  , CallT(SVar("e_19"), [], [])
                                  , CallT(SVar("f_19"), [], [])
                                  ]
                                , []
                                )
                              , Seq(
                                  Match(Var("h_19"))
                                , Seq(
                                    Build(Var("j_19"))
                                  , Seq(
                                      Build(
                                        Anno(
                                          Op("", [Var("g_19"), Var("h_19")])
                                        , Op("Nil", [])
                                        )
                                      )
                                    , CallT(SVar("e_19"), [], [])
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "conc_0_0"
        , []
        , []
        , GuardedLChoice(
            Scope(
              ["k_19", "l_19"]
            , Seq(
                Match(
                  Anno(
                    Op("", [Var("k_19"), Var("l_19")])
                  , Wld()
                  )
                )
              , Seq(
                  Build(Var("k_19"))
                , CallT(SVar("at_end_1_0"), [Build(Var("l_19"))], [])
                )
              )
            )
          , Id()
          , Scope(
              ["m_19"]
            , Seq(
                Match(
                  Anno(
                    Explode(Anno(Str(""), Wld()), Var("m_19"))
                  , Wld()
                  )
                )
              , Seq(
                  Build(Var("m_19"))
                , CallT(SVar("concat_0_0"), [], [])
                )
              )
            )
          )
        )
      , SDefT(
          "at_end_1_0"
        , [ VarDec(
              "s_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "t_19"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["n_19", "o_19", "p_19", "q_19", "r_19"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("n_19"), Var("o_19")])
                        , Var("r_19")
                        )
                      )
                    , Seq(
                        Build(Var("n_19"))
                      , Seq(
                          Match(Var("p_19"))
                        , Seq(
                            Build(Var("o_19"))
                          , Seq(
                              CallT(SVar("t_19"), [], [])
                            , Seq(
                                Match(Var("q_19"))
                              , Build(
                                  Anno(
                                    Op("Cons", [Var("p_19"), Var("q_19")])
                                  , Var("r_19")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                , Id()
                , Seq(
                    Match(Anno(Op("Nil", []), Wld()))
                  , CallT(SVar("s_19"), [], [])
                  )
                )
              )
            ]
          , CallT(SVar("t_19"), [], [])
          )
        )
      , SDefT(
          "concat_0_0"
        , []
        , []
        , Let(
            [ SDefT(
                "w_19"
              , []
              , []
              , GuardedLChoice(
                  Match(Anno(Op("Nil", []), Wld()))
                , Id()
                , Scope(
                    ["u_19", "v_19"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("u_19"), Var("v_19")])
                        , Wld()
                        )
                      )
                    , Seq(
                        Build(Var("u_19"))
                      , CallT(
                          SVar("at_end_1_0")
                        , [Seq(
                             Build(Var("v_19"))
                           , CallT(SVar("w_19"), [], [])
                           )]
                        , []
                        )
                      )
                    )
                  )
                )
              )
            ]
          , CallT(SVar("w_19"), [], [])
          )
        )
      , SDefT(
          "union_0_0"
        , []
        , []
        , Scope(
            ["x_19", "y_19"]
          , Let(
              [ SDefT(
                  "e_20"
                , []
                , []
                , GuardedLChoice(
                    Seq(
                      Match(Anno(Op("Nil", []), Wld()))
                    , Build(Var("x_19"))
                    )
                  , Id()
                  , GuardedLChoice(
                      Seq(
                        CallT(SVar("HdMember_1_0"), [Build(Var("x_19"))], [])
                      , CallT(SVar("e_20"), [], [])
                      )
                    , Id()
                    , Scope(
                        ["z_19", "a_20", "b_20", "c_20", "d_20"]
                      , Seq(
                          Match(
                            Anno(
                              Op("Cons", [Var("z_19"), Var("a_20")])
                            , Var("d_20")
                            )
                          )
                        , Seq(
                            Build(Var("z_19"))
                          , Seq(
                              Match(Var("b_20"))
                            , Seq(
                                Build(Var("a_20"))
                              , Seq(
                                  CallT(SVar("e_20"), [], [])
                                , Seq(
                                    Match(Var("c_20"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("b_20"), Var("c_20")])
                                      , Var("d_20")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ]
            , Seq(
                Match(
                  Anno(
                    Op("", [Var("y_19"), Var("x_19")])
                  , Wld()
                  )
                )
              , Seq(
                  Build(Var("y_19"))
                , CallT(SVar("e_20"), [], [])
                )
              )
            )
          )
        )
      , SDefT(
          "HdMember_1_0"
        , [ VarDec(
              "i_20"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_20", "g_20", "j_20"]
          , Seq(
              Match(
                Anno(
                  Op("Cons", [Var("g_20"), Var("f_20")])
                , Wld()
                )
              )
            , Seq(
                Match(Var("j_20"))
              , Seq(
                  CallT(SVar("i_20"), [], [])
                , Seq(
                    CallT(
                      SVar("fetch_1_0")
                    , [ Scope(
                          ["h_20"]
                        , Seq(
                            Match(Var("h_20"))
                          , Seq(
                              Build(
                                Anno(
                                  Op("", [Var("g_20"), Var("h_20")])
                                , Op("Nil", [])
                                )
                              )
                            , CallT(SVar("eq_0_0"), [], [])
                            )
                          )
                        )
                      ]
                    , []
                    )
                  , Seq(Build(Var("j_20")), Build(Var("f_20")))
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "fetch_1_0"
        , [ VarDec(
              "u_20"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "v_20"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["k_20", "l_20", "m_20", "n_20", "o_20"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("k_20"), Var("l_20")])
                        , Var("o_20")
                        )
                      )
                    , Seq(
                        Build(Var("k_20"))
                      , Seq(
                          CallT(SVar("u_20"), [], [])
                        , Seq(
                            Match(Var("m_20"))
                          , Seq(
                              Build(Var("l_20"))
                            , Seq(
                                Match(Var("n_20"))
                              , Build(
                                  Anno(
                                    Op("Cons", [Var("m_20"), Var("n_20")])
                                  , Var("o_20")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                , Id()
                , Scope(
                    ["p_20", "q_20", "r_20", "s_20", "t_20"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("p_20"), Var("q_20")])
                        , Var("t_20")
                        )
                      )
                    , Seq(
                        Build(Var("p_20"))
                      , Seq(
                          Match(Var("r_20"))
                        , Seq(
                            Build(Var("q_20"))
                          , Seq(
                              CallT(SVar("v_20"), [], [])
                            , Seq(
                                Match(Var("s_20"))
                              , Build(
                                  Anno(
                                    Op("Cons", [Var("r_20"), Var("s_20")])
                                  , Var("t_20")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ]
          , CallT(SVar("v_20"), [], [])
          )
        )
      , SDefT(
          "eq_0_0"
        , []
        , []
        , Scope(
            ["w_20"]
          , Match(
              Anno(
                Op("", [Var("w_20"), Var("w_20")])
              , Wld()
              )
            )
          )
        )
      , SDefT(
          "oncetd_1_0"
        , [ VarDec(
              "x_20"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "y_20"
              , []
              , []
              , GuardedLChoice(
                  CallT(SVar("x_20"), [], [])
                , Id()
                , One(CallT(SVar("y_20"), [], []))
                )
              )
            ]
          , CallT(SVar("y_20"), [], [])
          )
        )
      , SDefT(
          "main_0_0"
        , []
        , []
        , CallT(
            SVar("oncetd_1_0")
          , [CallT(SVar("desugar_arrow_0_0"), [], [])]
          , []
          )
        )
      , SDefT(
          "Anno__Cong_____2_0"
        , [ VarDec(
              "d_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_20", "a_21", "b_21", "c_21"]
          , Seq(
              Match(Anno(Var("z_20"), Var("a_21")))
            , Seq(
                Build(Var("z_20"))
              , Seq(
                  CallT(SVar("d_21"), [], [])
                , Seq(
                    Match(Var("b_21"))
                  , Seq(
                      Build(Var("a_21"))
                    , Seq(
                        CallT(SVar("e_21"), [], [])
                      , Seq(
                          Match(Var("c_21"))
                        , Build(Anno(Var("b_21"), Var("c_21")))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Nil_0_0"
        , []
        , []
        , Match(Anno(Op("Nil", []), Wld()))
        )
      , SDefT(
          "Cons_2_0"
        , [ VarDec(
              "f_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_32", "q_32", "r_32", "t_32", "u_32"]
          , Seq(
              Match(
                Anno(
                  Op("Cons", [Var("q_32"), Var("r_32")])
                , Var("s_32")
                )
              )
            , Seq(
                Build(Var("q_32"))
              , Seq(
                  CallT(SVar("f_21"), [], [])
                , Seq(
                    Match(Var("t_32"))
                  , Seq(
                      Build(Var("r_32"))
                    , Seq(
                        CallT(SVar("g_21"), [], [])
                      , Seq(
                          Match(Var("u_32"))
                        , Build(
                            Anno(
                              Op("Cons", [Var("t_32"), Var("u_32")])
                            , Var("s_32")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Conc_2_0"
        , [ VarDec(
              "h_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_32", "v_32", "w_32", "y_32", "z_32"]
          , Seq(
              Match(
                Anno(
                  Op("Conc", [Var("v_32"), Var("w_32")])
                , Var("x_32")
                )
              )
            , Seq(
                Build(Var("v_32"))
              , Seq(
                  CallT(SVar("h_21"), [], [])
                , Seq(
                    Match(Var("y_32"))
                  , Seq(
                      Build(Var("w_32"))
                    , Seq(
                        CallT(SVar("i_21"), [], [])
                      , Seq(
                          Match(Var("z_32"))
                        , Build(
                            Anno(
                              Op("Conc", [Var("y_32"), Var("z_32")])
                            , Var("x_32")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "_0_0"
        , []
        , []
        , Match(Anno(Op("", []), Wld()))
        )
      , SDefT(
          "_2_0"
        , [ VarDec(
              "j_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_33", "a_33", "b_33", "d_33", "e_33"]
          , Seq(
              Match(
                Anno(
                  Op("", [Var("a_33"), Var("b_33")])
                , Var("c_33")
                )
              )
            , Seq(
                Build(Var("a_33"))
              , Seq(
                  CallT(SVar("j_21"), [], [])
                , Seq(
                    Match(Var("d_33"))
                  , Seq(
                      Build(Var("b_33"))
                    , Seq(
                        CallT(SVar("k_21"), [], [])
                      , Seq(
                          Match(Var("e_33"))
                        , Build(
                            Anno(
                              Op("", [Var("d_33"), Var("e_33")])
                            , Var("c_33")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "_3_0"
        , [ VarDec(
              "l_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_33", "f_33", "g_33", "h_33", "j_33", "k_33", "l_33"]
          , Seq(
              Match(
                Anno(
                  Op(
                    ""
                  , [Var("f_33"), Var("g_33"), Var("h_33")]
                  )
                , Var("i_33")
                )
              )
            , Seq(
                Build(Var("f_33"))
              , Seq(
                  CallT(SVar("l_21"), [], [])
                , Seq(
                    Match(Var("j_33"))
                  , Seq(
                      Build(Var("g_33"))
                    , Seq(
                        CallT(SVar("m_21"), [], [])
                      , Seq(
                          Match(Var("k_33"))
                        , Seq(
                            Build(Var("h_33"))
                          , Seq(
                              CallT(SVar("n_21"), [], [])
                            , Seq(
                                Match(Var("l_33"))
                              , Build(
                                  Anno(
                                    Op(
                                      ""
                                    , [Var("j_33"), Var("k_33"), Var("l_33")]
                                    )
                                  , Var("i_33")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "StmtSeqOff_2_0"
        , [ VarDec(
              "o_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_33", "m_33", "n_33", "p_33", "q_33"]
          , Seq(
              Match(
                Anno(
                  Op("StmtSeqOff", [Var("m_33"), Var("n_33")])
                , Var("o_33")
                )
              )
            , Seq(
                Build(Var("m_33"))
              , Seq(
                  CallT(SVar("o_21"), [], [])
                , Seq(
                    Match(Var("p_33"))
                  , Seq(
                      Build(Var("n_33"))
                    , Seq(
                        CallT(SVar("p_21"), [], [])
                      , Seq(
                          Match(Var("q_33"))
                        , Build(
                            Anno(
                              Op("StmtSeqOff", [Var("p_33"), Var("q_33")])
                            , Var("o_33")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DeclSeqOff_2_0"
        , [ VarDec(
              "q_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_33", "r_33", "s_33", "u_33", "v_33"]
          , Seq(
              Match(
                Anno(
                  Op("DeclSeqOff", [Var("r_33"), Var("s_33")])
                , Var("t_33")
                )
              )
            , Seq(
                Build(Var("r_33"))
              , Seq(
                  CallT(SVar("q_21"), [], [])
                , Seq(
                    Match(Var("u_33"))
                  , Seq(
                      Build(Var("s_33"))
                    , Seq(
                        CallT(SVar("r_21"), [], [])
                      , Seq(
                          Match(Var("v_33"))
                        , Build(
                            Anno(
                              Op("DeclSeqOff", [Var("u_33"), Var("v_33")])
                            , Var("t_33")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "AltSeqOff_2_0"
        , [ VarDec(
              "s_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_33", "w_33", "x_33", "z_33", "a_34"]
          , Seq(
              Match(
                Anno(
                  Op("AltSeqOff", [Var("w_33"), Var("x_33")])
                , Var("y_33")
                )
              )
            , Seq(
                Build(Var("w_33"))
              , Seq(
                  CallT(SVar("s_21"), [], [])
                , Seq(
                    Match(Var("z_33"))
                  , Seq(
                      Build(Var("x_33"))
                    , Seq(
                        CallT(SVar("t_21"), [], [])
                      , Seq(
                          Match(Var("a_34"))
                        , Build(
                            Anno(
                              Op("AltSeqOff", [Var("z_33"), Var("a_34")])
                            , Var("y_33")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TopdeclSeqOff_2_0"
        , [ VarDec(
              "u_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_34", "b_34", "c_34", "e_34", "f_34"]
          , Seq(
              Match(
                Anno(
                  Op("TopdeclSeqOff", [Var("b_34"), Var("c_34")])
                , Var("d_34")
                )
              )
            , Seq(
                Build(Var("b_34"))
              , Seq(
                  CallT(SVar("u_21"), [], [])
                , Seq(
                    Match(Var("e_34"))
                  , Seq(
                      Build(Var("c_34"))
                    , Seq(
                        CallT(SVar("v_21"), [], [])
                      , Seq(
                          Match(Var("f_34"))
                        , Build(
                            Anno(
                              Op("TopdeclSeqOff", [Var("e_34"), Var("f_34")])
                            , Var("d_34")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ImportdeclSeqOff_2_0"
        , [ VarDec(
              "w_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_34", "g_34", "h_34", "j_34", "k_34"]
          , Seq(
              Match(
                Anno(
                  Op("ImportdeclSeqOff", [Var("g_34"), Var("h_34")])
                , Var("i_34")
                )
              )
            , Seq(
                Build(Var("g_34"))
              , Seq(
                  CallT(SVar("w_21"), [], [])
                , Seq(
                    Match(Var("j_34"))
                  , Seq(
                      Build(Var("h_34"))
                    , Seq(
                        CallT(SVar("x_21"), [], [])
                      , Seq(
                          Match(Var("k_34"))
                        , Build(
                            Anno(
                              Op("ImportdeclSeqOff", [Var("j_34"), Var("k_34")])
                            , Var("i_34")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "FloatHash_1_0"
        , [ VarDec(
              "y_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_34", "l_34", "n_34"]
          , Seq(
              Match(
                Anno(Op("FloatHash", [Var("l_34")]), Var("m_34"))
              )
            , Seq(
                Build(Var("l_34"))
              , Seq(
                  CallT(SVar("y_21"), [], [])
                , Seq(
                    Match(Var("n_34"))
                  , Build(
                      Anno(Op("FloatHash", [Var("n_34")]), Var("m_34"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "IntegerHash_1_0"
        , [ VarDec(
              "z_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_34", "o_34", "q_34"]
          , Seq(
              Match(
                Anno(Op("IntegerHash", [Var("o_34")]), Var("p_34"))
              )
            , Seq(
                Build(Var("o_34"))
              , Seq(
                  CallT(SVar("z_21"), [], [])
                , Seq(
                    Match(Var("q_34"))
                  , Build(
                      Anno(Op("IntegerHash", [Var("q_34")]), Var("p_34"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "StringHash_1_0"
        , [ VarDec(
              "a_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_34", "r_34", "t_34"]
          , Seq(
              Match(
                Anno(Op("StringHash", [Var("r_34")]), Var("s_34"))
              )
            , Seq(
                Build(Var("r_34"))
              , Seq(
                  CallT(SVar("a_22"), [], [])
                , Seq(
                    Match(Var("t_34"))
                  , Build(
                      Anno(Op("StringHash", [Var("t_34")]), Var("s_34"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "CharHash_1_0"
        , [ VarDec(
              "b_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_34", "u_34", "w_34"]
          , Seq(
              Match(
                Anno(Op("CharHash", [Var("u_34")]), Var("v_34"))
              )
            , Seq(
                Build(Var("u_34"))
              , Seq(
                  CallT(SVar("b_22"), [], [])
                , Seq(
                    Match(Var("w_34"))
                  , Build(
                      Anno(Op("CharHash", [Var("w_34")]), Var("v_34"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "FlexibleContext_1_0"
        , [ VarDec(
              "c_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_34", "x_34", "z_34"]
          , Seq(
              Match(
                Anno(Op("FlexibleContext", [Var("x_34")]), Var("y_34"))
              )
            , Seq(
                Build(Var("x_34"))
              , Seq(
                  CallT(SVar("c_22"), [], [])
                , Seq(
                    Match(Var("z_34"))
                  , Build(
                      Anno(Op("FlexibleContext", [Var("z_34")]), Var("y_34"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "SimpleClassFle_2_0"
        , [ VarDec(
              "d_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_35", "a_35", "b_35", "d_35", "e_35"]
          , Seq(
              Match(
                Anno(
                  Op("SimpleClassFle", [Var("a_35"), Var("b_35")])
                , Var("c_35")
                )
              )
            , Seq(
                Build(Var("a_35"))
              , Seq(
                  CallT(SVar("d_22"), [], [])
                , Seq(
                    Match(Var("d_35"))
                  , Seq(
                      Build(Var("b_35"))
                    , Seq(
                        CallT(SVar("e_22"), [], [])
                      , Seq(
                          Match(Var("e_35"))
                        , Build(
                            Anno(
                              Op("SimpleClassFle", [Var("d_35"), Var("e_35")])
                            , Var("c_35")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ClassFlex_2_0"
        , [ VarDec(
              "f_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_35", "f_35", "g_35", "i_35", "j_35"]
          , Seq(
              Match(
                Anno(
                  Op("ClassFlex", [Var("f_35"), Var("g_35")])
                , Var("h_35")
                )
              )
            , Seq(
                Build(Var("f_35"))
              , Seq(
                  CallT(SVar("f_22"), [], [])
                , Seq(
                    Match(Var("i_35"))
                  , Seq(
                      Build(Var("g_35"))
                    , Seq(
                        CallT(SVar("g_22"), [], [])
                      , Seq(
                          Match(Var("j_35"))
                        , Build(
                            Anno(
                              Op("ClassFlex", [Var("i_35"), Var("j_35")])
                            , Var("h_35")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "StmtSeq_2_0"
        , [ VarDec(
              "h_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_35", "k_35", "l_35", "n_35", "o_35"]
          , Seq(
              Match(
                Anno(
                  Op("StmtSeq", [Var("k_35"), Var("l_35")])
                , Var("m_35")
                )
              )
            , Seq(
                Build(Var("k_35"))
              , Seq(
                  CallT(SVar("h_22"), [], [])
                , Seq(
                    Match(Var("n_35"))
                  , Seq(
                      Build(Var("l_35"))
                    , Seq(
                        CallT(SVar("i_22"), [], [])
                      , Seq(
                          Match(Var("o_35"))
                        , Build(
                            Anno(
                              Op("StmtSeq", [Var("n_35"), Var("o_35")])
                            , Var("m_35")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "StmtList_1_0"
        , [ VarDec(
              "j_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_35", "p_35", "r_35"]
          , Seq(
              Match(
                Anno(Op("StmtList", [Var("p_35")]), Var("q_35"))
              )
            , Seq(
                Build(Var("p_35"))
              , Seq(
                  CallT(SVar("j_22"), [], [])
                , Seq(
                    Match(Var("r_35"))
                  , Build(
                      Anno(Op("StmtList", [Var("r_35")]), Var("q_35"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "FBind_2_0"
        , [ VarDec(
              "k_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "l_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_35", "s_35", "t_35", "v_35", "w_35"]
          , Seq(
              Match(
                Anno(
                  Op("FBind", [Var("s_35"), Var("t_35")])
                , Var("u_35")
                )
              )
            , Seq(
                Build(Var("s_35"))
              , Seq(
                  CallT(SVar("k_22"), [], [])
                , Seq(
                    Match(Var("v_35"))
                  , Seq(
                      Build(Var("t_35"))
                    , Seq(
                        CallT(SVar("l_22"), [], [])
                      , Seq(
                          Match(Var("w_35"))
                        , Build(
                            Anno(
                              Op("FBind", [Var("v_35"), Var("w_35")])
                            , Var("u_35")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "LetStmt_1_0"
        , [ VarDec(
              "m_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_35", "x_35", "z_35"]
          , Seq(
              Match(
                Anno(Op("LetStmt", [Var("x_35")]), Var("y_35"))
              )
            , Seq(
                Build(Var("x_35"))
              , Seq(
                  CallT(SVar("m_22"), [], [])
                , Seq(
                    Match(Var("z_35"))
                  , Build(
                      Anno(Op("LetStmt", [Var("z_35")]), Var("y_35"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ExpStmt_1_0"
        , [ VarDec(
              "n_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_36", "a_36", "c_36"]
          , Seq(
              Match(
                Anno(Op("ExpStmt", [Var("a_36")]), Var("b_36"))
              )
            , Seq(
                Build(Var("a_36"))
              , Seq(
                  CallT(SVar("n_22"), [], [])
                , Seq(
                    Match(Var("c_36"))
                  , Build(
                      Anno(Op("ExpStmt", [Var("c_36")]), Var("b_36"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "BindStmt_2_0"
        , [ VarDec(
              "o_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_36", "d_36", "e_36", "g_36", "h_36"]
          , Seq(
              Match(
                Anno(
                  Op("BindStmt", [Var("d_36"), Var("e_36")])
                , Var("f_36")
                )
              )
            , Seq(
                Build(Var("d_36"))
              , Seq(
                  CallT(SVar("o_22"), [], [])
                , Seq(
                    Match(Var("g_36"))
                  , Seq(
                      Build(Var("e_36"))
                    , Seq(
                        CallT(SVar("p_22"), [], [])
                      , Seq(
                          Match(Var("h_36"))
                        , Build(
                            Anno(
                              Op("BindStmt", [Var("g_36"), Var("h_36")])
                            , Var("f_36")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ListCompr_2_0"
        , [ VarDec(
              "q_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_36", "i_36", "j_36", "l_36", "m_36"]
          , Seq(
              Match(
                Anno(
                  Op("ListCompr", [Var("i_36"), Var("j_36")])
                , Var("k_36")
                )
              )
            , Seq(
                Build(Var("i_36"))
              , Seq(
                  CallT(SVar("q_22"), [], [])
                , Seq(
                    Match(Var("l_36"))
                  , Seq(
                      Build(Var("j_36"))
                    , Seq(
                        CallT(SVar("r_22"), [], [])
                      , Seq(
                          Match(Var("m_36"))
                        , Build(
                            Anno(
                              Op("ListCompr", [Var("l_36"), Var("m_36")])
                            , Var("k_36")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ListFirstFromTo_3_0"
        , [ VarDec(
              "s_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_36", "n_36", "o_36", "p_36", "r_36", "s_36", "t_36"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ListFirstFromTo"
                  , [Var("n_36"), Var("o_36"), Var("p_36")]
                  )
                , Var("q_36")
                )
              )
            , Seq(
                Build(Var("n_36"))
              , Seq(
                  CallT(SVar("s_22"), [], [])
                , Seq(
                    Match(Var("r_36"))
                  , Seq(
                      Build(Var("o_36"))
                    , Seq(
                        CallT(SVar("t_22"), [], [])
                      , Seq(
                          Match(Var("s_36"))
                        , Seq(
                            Build(Var("p_36"))
                          , Seq(
                              CallT(SVar("u_22"), [], [])
                            , Seq(
                                Match(Var("t_36"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ListFirstFromTo"
                                    , [Var("r_36"), Var("s_36"), Var("t_36")]
                                    )
                                  , Var("q_36")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ListFromTo_2_0"
        , [ VarDec(
              "v_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_36", "u_36", "v_36", "x_36", "y_36"]
          , Seq(
              Match(
                Anno(
                  Op("ListFromTo", [Var("u_36"), Var("v_36")])
                , Var("w_36")
                )
              )
            , Seq(
                Build(Var("u_36"))
              , Seq(
                  CallT(SVar("v_22"), [], [])
                , Seq(
                    Match(Var("x_36"))
                  , Seq(
                      Build(Var("v_36"))
                    , Seq(
                        CallT(SVar("w_22"), [], [])
                      , Seq(
                          Match(Var("y_36"))
                        , Build(
                            Anno(
                              Op("ListFromTo", [Var("x_36"), Var("y_36")])
                            , Var("w_36")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ListFirstFrom_2_0"
        , [ VarDec(
              "x_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_37", "z_36", "a_37", "c_37", "d_37"]
          , Seq(
              Match(
                Anno(
                  Op("ListFirstFrom", [Var("z_36"), Var("a_37")])
                , Var("b_37")
                )
              )
            , Seq(
                Build(Var("z_36"))
              , Seq(
                  CallT(SVar("x_22"), [], [])
                , Seq(
                    Match(Var("c_37"))
                  , Seq(
                      Build(Var("a_37"))
                    , Seq(
                        CallT(SVar("y_22"), [], [])
                      , Seq(
                          Match(Var("d_37"))
                        , Build(
                            Anno(
                              Op("ListFirstFrom", [Var("c_37"), Var("d_37")])
                            , Var("b_37")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ListFrom_1_0"
        , [ VarDec(
              "z_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_37", "e_37", "g_37"]
          , Seq(
              Match(
                Anno(Op("ListFrom", [Var("e_37")]), Var("f_37"))
              )
            , Seq(
                Build(Var("e_37"))
              , Seq(
                  CallT(SVar("z_22"), [], [])
                , Seq(
                    Match(Var("g_37"))
                  , Build(
                      Anno(Op("ListFrom", [Var("g_37")]), Var("f_37"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "List_1_0"
        , [ VarDec(
              "a_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_37", "h_37", "j_37"]
          , Seq(
              Match(
                Anno(Op("List", [Var("h_37")]), Var("i_37"))
              )
            , Seq(
                Build(Var("h_37"))
              , Seq(
                  CallT(SVar("a_23"), [], [])
                , Seq(
                    Match(Var("j_37"))
                  , Build(
                      Anno(Op("List", [Var("j_37")]), Var("i_37"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QualLet_1_0"
        , [ VarDec(
              "b_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_37", "k_37", "m_37"]
          , Seq(
              Match(
                Anno(Op("QualLet", [Var("k_37")]), Var("l_37"))
              )
            , Seq(
                Build(Var("k_37"))
              , Seq(
                  CallT(SVar("b_23"), [], [])
                , Seq(
                    Match(Var("m_37"))
                  , Build(
                      Anno(Op("QualLet", [Var("m_37")]), Var("l_37"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QualBind_2_0"
        , [ VarDec(
              "c_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_37", "n_37", "o_37", "q_37", "r_37"]
          , Seq(
              Match(
                Anno(
                  Op("QualBind", [Var("n_37"), Var("o_37")])
                , Var("p_37")
                )
              )
            , Seq(
                Build(Var("n_37"))
              , Seq(
                  CallT(SVar("c_23"), [], [])
                , Seq(
                    Match(Var("q_37"))
                  , Seq(
                      Build(Var("o_37"))
                    , Seq(
                        CallT(SVar("d_23"), [], [])
                      , Seq(
                          Match(Var("r_37"))
                        , Build(
                            Anno(
                              Op("QualBind", [Var("q_37"), Var("r_37")])
                            , Var("p_37")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PatBind_2_0"
        , [ VarDec(
              "e_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_37", "s_37", "t_37", "v_37", "w_37"]
          , Seq(
              Match(
                Anno(
                  Op("PatBind", [Var("s_37"), Var("t_37")])
                , Var("u_37")
                )
              )
            , Seq(
                Build(Var("s_37"))
              , Seq(
                  CallT(SVar("e_23"), [], [])
                , Seq(
                    Match(Var("v_37"))
                  , Seq(
                      Build(Var("t_37"))
                    , Seq(
                        CallT(SVar("f_23"), [], [])
                      , Seq(
                          Match(Var("w_37"))
                        , Build(
                            Anno(
                              Op("PatBind", [Var("v_37"), Var("w_37")])
                            , Var("u_37")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "LabeledPats_1_0"
        , [ VarDec(
              "g_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_37", "x_37", "z_37"]
          , Seq(
              Match(
                Anno(Op("LabeledPats", [Var("x_37")]), Var("y_37"))
              )
            , Seq(
                Build(Var("x_37"))
              , Seq(
                  CallT(SVar("g_23"), [], [])
                , Seq(
                    Match(Var("z_37"))
                  , Build(
                      Anno(Op("LabeledPats", [Var("z_37")]), Var("y_37"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Irrefutable_1_0"
        , [ VarDec(
              "h_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_38", "a_38", "c_38"]
          , Seq(
              Match(
                Anno(Op("Irrefutable", [Var("a_38")]), Var("b_38"))
              )
            , Seq(
                Build(Var("a_38"))
              , Seq(
                  CallT(SVar("h_23"), [], [])
                , Seq(
                    Match(Var("c_38"))
                  , Build(
                      Anno(Op("Irrefutable", [Var("c_38")]), Var("b_38"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ListPat_1_0"
        , [ VarDec(
              "i_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_38", "d_38", "f_38"]
          , Seq(
              Match(
                Anno(Op("ListPat", [Var("d_38")]), Var("e_38"))
              )
            , Seq(
                Build(Var("d_38"))
              , Seq(
                  CallT(SVar("i_23"), [], [])
                , Seq(
                    Match(Var("f_38"))
                  , Build(
                      Anno(Op("ListPat", [Var("f_38")]), Var("e_38"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TuplePat_2_0"
        , [ VarDec(
              "j_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_38", "g_38", "h_38", "j_38", "k_38"]
          , Seq(
              Match(
                Anno(
                  Op("TuplePat", [Var("g_38"), Var("h_38")])
                , Var("i_38")
                )
              )
            , Seq(
                Build(Var("g_38"))
              , Seq(
                  CallT(SVar("j_23"), [], [])
                , Seq(
                    Match(Var("j_38"))
                  , Seq(
                      Build(Var("h_38"))
                    , Seq(
                        CallT(SVar("k_23"), [], [])
                      , Seq(
                          Match(Var("k_38"))
                        , Build(
                            Anno(
                              Op("TuplePat", [Var("j_38"), Var("k_38")])
                            , Var("i_38")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Wildcard_0_0"
        , []
        , []
        , Match(Anno(Op("Wildcard", []), Wld()))
        )
      , SDefT(
          "LabeledPat_2_0"
        , [ VarDec(
              "l_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_38", "l_38", "m_38", "o_38", "p_38"]
          , Seq(
              Match(
                Anno(
                  Op("LabeledPat", [Var("l_38"), Var("m_38")])
                , Var("n_38")
                )
              )
            , Seq(
                Build(Var("l_38"))
              , Seq(
                  CallT(SVar("l_23"), [], [])
                , Seq(
                    Match(Var("o_38"))
                  , Seq(
                      Build(Var("m_38"))
                    , Seq(
                        CallT(SVar("m_23"), [], [])
                      , Seq(
                          Match(Var("p_38"))
                        , Build(
                            Anno(
                              Op("LabeledPat", [Var("o_38"), Var("p_38")])
                            , Var("n_38")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ConstrPat_1_0"
        , [ VarDec(
              "n_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_38", "q_38", "s_38"]
          , Seq(
              Match(
                Anno(Op("ConstrPat", [Var("q_38")]), Var("r_38"))
              )
            , Seq(
                Build(Var("q_38"))
              , Seq(
                  CallT(SVar("n_23"), [], [])
                , Seq(
                    Match(Var("s_38"))
                  , Build(
                      Anno(Op("ConstrPat", [Var("s_38")]), Var("r_38"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "NamedPat_2_0"
        , [ VarDec(
              "o_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_38", "t_38", "u_38", "w_38", "x_38"]
          , Seq(
              Match(
                Anno(
                  Op("NamedPat", [Var("t_38"), Var("u_38")])
                , Var("v_38")
                )
              )
            , Seq(
                Build(Var("t_38"))
              , Seq(
                  CallT(SVar("o_23"), [], [])
                , Seq(
                    Match(Var("w_38"))
                  , Seq(
                      Build(Var("u_38"))
                    , Seq(
                        CallT(SVar("p_23"), [], [])
                      , Seq(
                          Match(Var("x_38"))
                        , Build(
                            Anno(
                              Op("NamedPat", [Var("w_38"), Var("x_38")])
                            , Var("v_38")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ConstrApp_2_0"
        , [ VarDec(
              "q_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_39", "y_38", "z_38", "b_39", "c_39"]
          , Seq(
              Match(
                Anno(
                  Op("ConstrApp", [Var("y_38"), Var("z_38")])
                , Var("a_39")
                )
              )
            , Seq(
                Build(Var("y_38"))
              , Seq(
                  CallT(SVar("q_23"), [], [])
                , Seq(
                    Match(Var("b_39"))
                  , Seq(
                      Build(Var("z_38"))
                    , Seq(
                        CallT(SVar("r_23"), [], [])
                      , Seq(
                          Match(Var("c_39"))
                        , Build(
                            Anno(
                              Op("ConstrApp", [Var("b_39"), Var("c_39")])
                            , Var("a_39")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "NegationPat_1_0"
        , [ VarDec(
              "s_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_39", "d_39", "f_39"]
          , Seq(
              Match(
                Anno(Op("NegationPat", [Var("d_39")]), Var("e_39"))
              )
            , Seq(
                Build(Var("d_39"))
              , Seq(
                  CallT(SVar("s_23"), [], [])
                , Seq(
                    Match(Var("f_39"))
                  , Build(
                      Anno(Op("NegationPat", [Var("f_39")]), Var("e_39"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "BinOpApp_3_0"
        , [ VarDec(
              "t_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_39", "g_39", "h_39", "i_39", "k_39", "l_39", "m_39"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "BinOpApp"
                  , [Var("g_39"), Var("h_39"), Var("i_39")]
                  )
                , Var("j_39")
                )
              )
            , Seq(
                Build(Var("g_39"))
              , Seq(
                  CallT(SVar("t_23"), [], [])
                , Seq(
                    Match(Var("k_39"))
                  , Seq(
                      Build(Var("h_39"))
                    , Seq(
                        CallT(SVar("u_23"), [], [])
                      , Seq(
                          Match(Var("l_39"))
                        , Seq(
                            Build(Var("i_39"))
                          , Seq(
                              CallT(SVar("v_23"), [], [])
                            , Seq(
                                Match(Var("m_39"))
                              , Build(
                                  Anno(
                                    Op(
                                      "BinOpApp"
                                    , [Var("k_39"), Var("l_39"), Var("m_39")]
                                    )
                                  , Var("j_39")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DeclSeq_2_0"
        , [ VarDec(
              "w_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_39", "n_39", "o_39", "q_39", "r_39"]
          , Seq(
              Match(
                Anno(
                  Op("DeclSeq", [Var("n_39"), Var("o_39")])
                , Var("p_39")
                )
              )
            , Seq(
                Build(Var("n_39"))
              , Seq(
                  CallT(SVar("w_23"), [], [])
                , Seq(
                    Match(Var("q_39"))
                  , Seq(
                      Build(Var("o_39"))
                    , Seq(
                        CallT(SVar("x_23"), [], [])
                      , Seq(
                          Match(Var("r_39"))
                        , Build(
                            Anno(
                              Op("DeclSeq", [Var("q_39"), Var("r_39")])
                            , Var("p_39")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DeclList_1_0"
        , [ VarDec(
              "y_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_39", "s_39", "u_39"]
          , Seq(
              Match(
                Anno(Op("DeclList", [Var("s_39")]), Var("t_39"))
              )
            , Seq(
                Build(Var("s_39"))
              , Seq(
                  CallT(SVar("y_23"), [], [])
                , Seq(
                    Match(Var("u_39"))
                  , Build(
                      Anno(Op("DeclList", [Var("u_39")]), Var("t_39"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Where_1_0"
        , [ VarDec(
              "z_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_39", "v_39", "x_39"]
          , Seq(
              Match(
                Anno(Op("Where", [Var("v_39")]), Var("w_39"))
              )
            , Seq(
                Build(Var("v_39"))
              , Seq(
                  CallT(SVar("z_23"), [], [])
                , Seq(
                    Match(Var("x_39"))
                  , Build(
                      Anno(Op("Where", [Var("x_39")]), Var("w_39"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "NestedFunLHS_2_0"
        , [ VarDec(
              "a_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_40", "y_39", "z_39", "b_40", "c_40"]
          , Seq(
              Match(
                Anno(
                  Op("NestedFunLHS", [Var("y_39"), Var("z_39")])
                , Var("a_40")
                )
              )
            , Seq(
                Build(Var("y_39"))
              , Seq(
                  CallT(SVar("a_24"), [], [])
                , Seq(
                    Match(Var("b_40"))
                  , Seq(
                      Build(Var("z_39"))
                    , Seq(
                        CallT(SVar("b_24"), [], [])
                      , Seq(
                          Match(Var("c_40"))
                        , Build(
                            Anno(
                              Op("NestedFunLHS", [Var("b_40"), Var("c_40")])
                            , Var("a_40")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "OpFunLHS_3_0"
        , [ VarDec(
              "c_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_40", "d_40", "e_40", "f_40", "h_40", "i_40", "j_40"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "OpFunLHS"
                  , [Var("d_40"), Var("e_40"), Var("f_40")]
                  )
                , Var("g_40")
                )
              )
            , Seq(
                Build(Var("d_40"))
              , Seq(
                  CallT(SVar("c_24"), [], [])
                , Seq(
                    Match(Var("h_40"))
                  , Seq(
                      Build(Var("e_40"))
                    , Seq(
                        CallT(SVar("d_24"), [], [])
                      , Seq(
                          Match(Var("i_40"))
                        , Seq(
                            Build(Var("f_40"))
                          , Seq(
                              CallT(SVar("e_24"), [], [])
                            , Seq(
                                Match(Var("j_40"))
                              , Build(
                                  Anno(
                                    Op(
                                      "OpFunLHS"
                                    , [Var("h_40"), Var("i_40"), Var("j_40")]
                                    )
                                  , Var("g_40")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "VarFunLHS_2_0"
        , [ VarDec(
              "f_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_40", "k_40", "l_40", "n_40", "o_40"]
          , Seq(
              Match(
                Anno(
                  Op("VarFunLHS", [Var("k_40"), Var("l_40")])
                , Var("m_40")
                )
              )
            , Seq(
                Build(Var("k_40"))
              , Seq(
                  CallT(SVar("f_24"), [], [])
                , Seq(
                    Match(Var("n_40"))
                  , Seq(
                      Build(Var("l_40"))
                    , Seq(
                        CallT(SVar("g_24"), [], [])
                      , Seq(
                          Match(Var("o_40"))
                        , Build(
                            Anno(
                              Op("VarFunLHS", [Var("n_40"), Var("o_40")])
                            , Var("m_40")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Guarded_2_0"
        , [ VarDec(
              "h_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_40", "p_40", "q_40", "s_40", "t_40"]
          , Seq(
              Match(
                Anno(
                  Op("Guarded", [Var("p_40"), Var("q_40")])
                , Var("r_40")
                )
              )
            , Seq(
                Build(Var("p_40"))
              , Seq(
                  CallT(SVar("h_24"), [], [])
                , Seq(
                    Match(Var("s_40"))
                  , Seq(
                      Build(Var("q_40"))
                    , Seq(
                        CallT(SVar("i_24"), [], [])
                      , Seq(
                          Match(Var("t_40"))
                        , Build(
                            Anno(
                              Op("Guarded", [Var("s_40"), Var("t_40")])
                            , Var("r_40")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "GdValdef_3_0"
        , [ VarDec(
              "j_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "l_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_40", "u_40", "v_40", "w_40", "y_40", "z_40", "a_41"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "GdValdef"
                  , [Var("u_40"), Var("v_40"), Var("w_40")]
                  )
                , Var("x_40")
                )
              )
            , Seq(
                Build(Var("u_40"))
              , Seq(
                  CallT(SVar("j_24"), [], [])
                , Seq(
                    Match(Var("y_40"))
                  , Seq(
                      Build(Var("v_40"))
                    , Seq(
                        CallT(SVar("k_24"), [], [])
                      , Seq(
                          Match(Var("z_40"))
                        , Seq(
                            Build(Var("w_40"))
                          , Seq(
                              CallT(SVar("l_24"), [], [])
                            , Seq(
                                Match(Var("a_41"))
                              , Build(
                                  Anno(
                                    Op(
                                      "GdValdef"
                                    , [Var("y_40"), Var("z_40"), Var("a_41")]
                                    )
                                  , Var("x_40")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Valdef_3_0"
        , [ VarDec(
              "m_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_41", "b_41", "c_41", "d_41", "f_41", "g_41", "h_41"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Valdef"
                  , [Var("b_41"), Var("c_41"), Var("d_41")]
                  )
                , Var("e_41")
                )
              )
            , Seq(
                Build(Var("b_41"))
              , Seq(
                  CallT(SVar("m_24"), [], [])
                , Seq(
                    Match(Var("f_41"))
                  , Seq(
                      Build(Var("c_41"))
                    , Seq(
                        CallT(SVar("n_24"), [], [])
                      , Seq(
                          Match(Var("g_41"))
                        , Seq(
                            Build(Var("d_41"))
                          , Seq(
                              CallT(SVar("o_24"), [], [])
                            , Seq(
                                Match(Var("h_41"))
                              , Build(
                                  Anno(
                                    Op(
                                      "Valdef"
                                    , [Var("f_41"), Var("g_41"), Var("h_41")]
                                    )
                                  , Var("e_41")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "AltSeq_2_0"
        , [ VarDec(
              "p_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_41", "i_41", "j_41", "l_41", "m_41"]
          , Seq(
              Match(
                Anno(
                  Op("AltSeq", [Var("i_41"), Var("j_41")])
                , Var("k_41")
                )
              )
            , Seq(
                Build(Var("i_41"))
              , Seq(
                  CallT(SVar("p_24"), [], [])
                , Seq(
                    Match(Var("l_41"))
                  , Seq(
                      Build(Var("j_41"))
                    , Seq(
                        CallT(SVar("q_24"), [], [])
                      , Seq(
                          Match(Var("m_41"))
                        , Build(
                            Anno(
                              Op("AltSeq", [Var("l_41"), Var("m_41")])
                            , Var("k_41")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "AltList_1_0"
        , [ VarDec(
              "r_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_41", "n_41", "p_41"]
          , Seq(
              Match(
                Anno(Op("AltList", [Var("n_41")]), Var("o_41"))
              )
            , Seq(
                Build(Var("n_41"))
              , Seq(
                  CallT(SVar("r_24"), [], [])
                , Seq(
                    Match(Var("p_41"))
                  , Build(
                      Anno(Op("AltList", [Var("p_41")]), Var("o_41"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "GdPat_2_0"
        , [ VarDec(
              "s_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_41", "q_41", "r_41", "t_41", "u_41"]
          , Seq(
              Match(
                Anno(
                  Op("GdPat", [Var("q_41"), Var("r_41")])
                , Var("s_41")
                )
              )
            , Seq(
                Build(Var("q_41"))
              , Seq(
                  CallT(SVar("s_24"), [], [])
                , Seq(
                    Match(Var("t_41"))
                  , Seq(
                      Build(Var("r_41"))
                    , Seq(
                        CallT(SVar("t_24"), [], [])
                      , Seq(
                          Match(Var("u_41"))
                        , Build(
                            Anno(
                              Op("GdPat", [Var("t_41"), Var("u_41")])
                            , Var("s_41")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "GdAlt_3_0"
        , [ VarDec(
              "u_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_41", "v_41", "w_41", "x_41", "z_41", "a_42", "b_42"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "GdAlt"
                  , [Var("v_41"), Var("w_41"), Var("x_41")]
                  )
                , Var("y_41")
                )
              )
            , Seq(
                Build(Var("v_41"))
              , Seq(
                  CallT(SVar("u_24"), [], [])
                , Seq(
                    Match(Var("z_41"))
                  , Seq(
                      Build(Var("w_41"))
                    , Seq(
                        CallT(SVar("v_24"), [], [])
                      , Seq(
                          Match(Var("a_42"))
                        , Seq(
                            Build(Var("x_41"))
                          , Seq(
                              CallT(SVar("w_24"), [], [])
                            , Seq(
                                Match(Var("b_42"))
                              , Build(
                                  Anno(
                                    Op(
                                      "GdAlt"
                                    , [Var("z_41"), Var("a_42"), Var("b_42")]
                                    )
                                  , Var("y_41")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Alt_3_0"
        , [ VarDec(
              "x_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_42", "c_42", "d_42", "e_42", "g_42", "h_42", "i_42"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Alt"
                  , [Var("c_42"), Var("d_42"), Var("e_42")]
                  )
                , Var("f_42")
                )
              )
            , Seq(
                Build(Var("c_42"))
              , Seq(
                  CallT(SVar("x_24"), [], [])
                , Seq(
                    Match(Var("g_42"))
                  , Seq(
                      Build(Var("d_42"))
                    , Seq(
                        CallT(SVar("y_24"), [], [])
                      , Seq(
                          Match(Var("h_42"))
                        , Seq(
                            Build(Var("e_42"))
                          , Seq(
                              CallT(SVar("z_24"), [], [])
                            , Seq(
                                Match(Var("i_42"))
                              , Build(
                                  Anno(
                                    Op(
                                      "Alt"
                                    , [Var("g_42"), Var("h_42"), Var("i_42")]
                                    )
                                  , Var("f_42")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "LabelBinds_1_0"
        , [ VarDec(
              "a_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_42", "j_42", "l_42"]
          , Seq(
              Match(
                Anno(Op("LabelBinds", [Var("j_42")]), Var("k_42"))
              )
            , Seq(
                Build(Var("j_42"))
              , Seq(
                  CallT(SVar("a_25"), [], [])
                , Seq(
                    Match(Var("l_42"))
                  , Build(
                      Anno(Op("LabelBinds", [Var("l_42")]), Var("k_42"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "FixDecl_3_0"
        , [ VarDec(
              "b_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "c_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_42", "m_42", "n_42", "o_42", "q_42", "r_42", "s_42"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "FixDecl"
                  , [Var("m_42"), Var("n_42"), Var("o_42")]
                  )
                , Var("p_42")
                )
              )
            , Seq(
                Build(Var("m_42"))
              , Seq(
                  CallT(SVar("b_25"), [], [])
                , Seq(
                    Match(Var("q_42"))
                  , Seq(
                      Build(Var("n_42"))
                    , Seq(
                        CallT(SVar("c_25"), [], [])
                      , Seq(
                          Match(Var("r_42"))
                        , Seq(
                            Build(Var("o_42"))
                          , Seq(
                              CallT(SVar("d_25"), [], [])
                            , Seq(
                                Match(Var("s_42"))
                              , Build(
                                  Anno(
                                    Op(
                                      "FixDecl"
                                    , [Var("q_42"), Var("r_42"), Var("s_42")]
                                    )
                                  , Var("p_42")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InfixR_0_0"
        , []
        , []
        , Match(Anno(Op("InfixR", []), Wld()))
        )
      , SDefT(
          "InfixL_0_0"
        , []
        , []
        , Match(Anno(Op("InfixL", []), Wld()))
        )
      , SDefT(
          "Infix_0_0"
        , []
        , []
        , Match(Anno(Op("Infix", []), Wld()))
        )
      , SDefT(
          "ECons_2_0"
        , [ VarDec(
              "e_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_42", "t_42", "u_42", "w_42", "x_42"]
          , Seq(
              Match(
                Anno(
                  Op("ECons", [Var("t_42"), Var("u_42")])
                , Var("v_42")
                )
              )
            , Seq(
                Build(Var("t_42"))
              , Seq(
                  CallT(SVar("e_25"), [], [])
                , Seq(
                    Match(Var("w_42"))
                  , Seq(
                      Build(Var("u_42"))
                    , Seq(
                        CallT(SVar("f_25"), [], [])
                      , Seq(
                          Match(Var("x_42"))
                        , Build(
                            Anno(
                              Op("ECons", [Var("w_42"), Var("x_42")])
                            , Var("v_42")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrOpApp_3_0"
        , [ VarDec(
              "g_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "h_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_43", "y_42", "z_42", "a_43", "c_43", "d_43", "e_43"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ArrOpApp"
                  , [Var("y_42"), Var("z_42"), Var("a_43")]
                  )
                , Var("b_43")
                )
              )
            , Seq(
                Build(Var("y_42"))
              , Seq(
                  CallT(SVar("g_25"), [], [])
                , Seq(
                    Match(Var("c_43"))
                  , Seq(
                      Build(Var("z_42"))
                    , Seq(
                        CallT(SVar("h_25"), [], [])
                      , Seq(
                          Match(Var("d_43"))
                        , Seq(
                            Build(Var("a_43"))
                          , Seq(
                              CallT(SVar("i_25"), [], [])
                            , Seq(
                                Match(Var("e_43"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ArrOpApp"
                                    , [Var("c_43"), Var("d_43"), Var("e_43")]
                                    )
                                  , Var("b_43")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrForm_2_0"
        , [ VarDec(
              "j_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_43", "f_43", "g_43", "i_43", "j_43"]
          , Seq(
              Match(
                Anno(
                  Op("ArrForm", [Var("f_43"), Var("g_43")])
                , Var("h_43")
                )
              )
            , Seq(
                Build(Var("f_43"))
              , Seq(
                  CallT(SVar("j_25"), [], [])
                , Seq(
                    Match(Var("i_43"))
                  , Seq(
                      Build(Var("g_43"))
                    , Seq(
                        CallT(SVar("k_25"), [], [])
                      , Seq(
                          Match(Var("j_43"))
                        , Build(
                            Anno(
                              Op("ArrForm", [Var("i_43"), Var("j_43")])
                            , Var("h_43")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrAppBin_2_0"
        , [ VarDec(
              "l_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_43", "k_43", "l_43", "n_43", "o_43"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAppBin", [Var("k_43"), Var("l_43")])
                , Var("m_43")
                )
              )
            , Seq(
                Build(Var("k_43"))
              , Seq(
                  CallT(SVar("l_25"), [], [])
                , Seq(
                    Match(Var("n_43"))
                  , Seq(
                      Build(Var("l_43"))
                    , Seq(
                        CallT(SVar("m_25"), [], [])
                      , Seq(
                          Match(Var("o_43"))
                        , Build(
                            Anno(
                              Op("ArrAppBin", [Var("n_43"), Var("o_43")])
                            , Var("m_43")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrDo_1_0"
        , [ VarDec(
              "n_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_43", "p_43", "r_43"]
          , Seq(
              Match(
                Anno(Op("ArrDo", [Var("p_43")]), Var("q_43"))
              )
            , Seq(
                Build(Var("p_43"))
              , Seq(
                  CallT(SVar("n_25"), [], [])
                , Seq(
                    Match(Var("r_43"))
                  , Build(
                      Anno(Op("ArrDo", [Var("r_43")]), Var("q_43"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrCase_2_0"
        , [ VarDec(
              "o_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_43", "s_43", "t_43", "v_43", "w_43"]
          , Seq(
              Match(
                Anno(
                  Op("ArrCase", [Var("s_43"), Var("t_43")])
                , Var("u_43")
                )
              )
            , Seq(
                Build(Var("s_43"))
              , Seq(
                  CallT(SVar("o_25"), [], [])
                , Seq(
                    Match(Var("v_43"))
                  , Seq(
                      Build(Var("t_43"))
                    , Seq(
                        CallT(SVar("p_25"), [], [])
                      , Seq(
                          Match(Var("w_43"))
                        , Build(
                            Anno(
                              Op("ArrCase", [Var("v_43"), Var("w_43")])
                            , Var("u_43")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrIf_3_0"
        , [ VarDec(
              "q_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "s_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_44", "x_43", "y_43", "z_43", "b_44", "c_44", "d_44"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ArrIf"
                  , [Var("x_43"), Var("y_43"), Var("z_43")]
                  )
                , Var("a_44")
                )
              )
            , Seq(
                Build(Var("x_43"))
              , Seq(
                  CallT(SVar("q_25"), [], [])
                , Seq(
                    Match(Var("b_44"))
                  , Seq(
                      Build(Var("y_43"))
                    , Seq(
                        CallT(SVar("r_25"), [], [])
                      , Seq(
                          Match(Var("c_44"))
                        , Seq(
                            Build(Var("z_43"))
                          , Seq(
                              CallT(SVar("s_25"), [], [])
                            , Seq(
                                Match(Var("d_44"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ArrIf"
                                    , [Var("b_44"), Var("c_44"), Var("d_44")]
                                    )
                                  , Var("a_44")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrLet_2_0"
        , [ VarDec(
              "t_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_44", "e_44", "f_44", "h_44", "i_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrLet", [Var("e_44"), Var("f_44")])
                , Var("g_44")
                )
              )
            , Seq(
                Build(Var("e_44"))
              , Seq(
                  CallT(SVar("t_25"), [], [])
                , Seq(
                    Match(Var("h_44"))
                  , Seq(
                      Build(Var("f_44"))
                    , Seq(
                        CallT(SVar("u_25"), [], [])
                      , Seq(
                          Match(Var("i_44"))
                        , Build(
                            Anno(
                              Op("ArrLet", [Var("h_44"), Var("i_44")])
                            , Var("g_44")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrAbs_2_0"
        , [ VarDec(
              "v_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_44", "j_44", "k_44", "m_44", "n_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAbs", [Var("j_44"), Var("k_44")])
                , Var("l_44")
                )
              )
            , Seq(
                Build(Var("j_44"))
              , Seq(
                  CallT(SVar("v_25"), [], [])
                , Seq(
                    Match(Var("m_44"))
                  , Seq(
                      Build(Var("k_44"))
                    , Seq(
                        CallT(SVar("w_25"), [], [])
                      , Seq(
                          Match(Var("n_44"))
                        , Build(
                            Anno(
                              Op("ArrAbs", [Var("m_44"), Var("n_44")])
                            , Var("l_44")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrHigher_2_0"
        , [ VarDec(
              "x_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_44", "o_44", "p_44", "r_44", "s_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrHigher", [Var("o_44"), Var("p_44")])
                , Var("q_44")
                )
              )
            , Seq(
                Build(Var("o_44"))
              , Seq(
                  CallT(SVar("x_25"), [], [])
                , Seq(
                    Match(Var("r_44"))
                  , Seq(
                      Build(Var("p_44"))
                    , Seq(
                        CallT(SVar("y_25"), [], [])
                      , Seq(
                          Match(Var("s_44"))
                        , Build(
                            Anno(
                              Op("ArrHigher", [Var("r_44"), Var("s_44")])
                            , Var("q_44")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrFirst_2_0"
        , [ VarDec(
              "z_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "a_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_44", "t_44", "u_44", "w_44", "x_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrFirst", [Var("t_44"), Var("u_44")])
                , Var("v_44")
                )
              )
            , Seq(
                Build(Var("t_44"))
              , Seq(
                  CallT(SVar("z_25"), [], [])
                , Seq(
                    Match(Var("w_44"))
                  , Seq(
                      Build(Var("u_44"))
                    , Seq(
                        CallT(SVar("a_26"), [], [])
                      , Seq(
                          Match(Var("x_44"))
                        , Build(
                            Anno(
                              Op("ArrFirst", [Var("w_44"), Var("x_44")])
                            , Var("v_44")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Typed_3_0"
        , [ VarDec(
              "b_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "c_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_45", "y_44", "z_44", "a_45", "c_45", "d_45", "e_45"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Typed"
                  , [Var("y_44"), Var("z_44"), Var("a_45")]
                  )
                , Var("b_45")
                )
              )
            , Seq(
                Build(Var("y_44"))
              , Seq(
                  CallT(SVar("b_26"), [], [])
                , Seq(
                    Match(Var("c_45"))
                  , Seq(
                      Build(Var("z_44"))
                    , Seq(
                        CallT(SVar("c_26"), [], [])
                      , Seq(
                          Match(Var("d_45"))
                        , Seq(
                            Build(Var("a_45"))
                          , Seq(
                              CallT(SVar("d_26"), [], [])
                            , Seq(
                                Match(Var("e_45"))
                              , Build(
                                  Anno(
                                    Op(
                                      "Typed"
                                    , [Var("c_45"), Var("d_45"), Var("e_45")]
                                    )
                                  , Var("b_45")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Negation_1_0"
        , [ VarDec(
              "e_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_45", "f_45", "h_45"]
          , Seq(
              Match(
                Anno(Op("Negation", [Var("f_45")]), Var("g_45"))
              )
            , Seq(
                Build(Var("f_45"))
              , Seq(
                  CallT(SVar("e_26"), [], [])
                , Seq(
                    Match(Var("h_45"))
                  , Build(
                      Anno(Op("Negation", [Var("h_45")]), Var("g_45"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Labeled_2_0"
        , [ VarDec(
              "f_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_45", "i_45", "j_45", "l_45", "m_45"]
          , Seq(
              Match(
                Anno(
                  Op("Labeled", [Var("i_45"), Var("j_45")])
                , Var("k_45")
                )
              )
            , Seq(
                Build(Var("i_45"))
              , Seq(
                  CallT(SVar("f_26"), [], [])
                , Seq(
                    Match(Var("l_45"))
                  , Seq(
                      Build(Var("j_45"))
                    , Seq(
                        CallT(SVar("g_26"), [], [])
                      , Seq(
                          Match(Var("m_45"))
                        , Build(
                            Anno(
                              Op("Labeled", [Var("l_45"), Var("m_45")])
                            , Var("k_45")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Named_2_0"
        , [ VarDec(
              "h_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_45", "n_45", "o_45", "q_45", "r_45"]
          , Seq(
              Match(
                Anno(
                  Op("Named", [Var("n_45"), Var("o_45")])
                , Var("p_45")
                )
              )
            , Seq(
                Build(Var("n_45"))
              , Seq(
                  CallT(SVar("h_26"), [], [])
                , Seq(
                    Match(Var("q_45"))
                  , Seq(
                      Build(Var("o_45"))
                    , Seq(
                        CallT(SVar("i_26"), [], [])
                      , Seq(
                          Match(Var("r_45"))
                        , Build(
                            Anno(
                              Op("Named", [Var("q_45"), Var("r_45")])
                            , Var("p_45")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "OpApp_3_0"
        , [ VarDec(
              "j_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "l_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_45", "s_45", "t_45", "u_45", "w_45", "x_45", "y_45"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "OpApp"
                  , [Var("s_45"), Var("t_45"), Var("u_45")]
                  )
                , Var("v_45")
                )
              )
            , Seq(
                Build(Var("s_45"))
              , Seq(
                  CallT(SVar("j_26"), [], [])
                , Seq(
                    Match(Var("w_45"))
                  , Seq(
                      Build(Var("t_45"))
                    , Seq(
                        CallT(SVar("k_26"), [], [])
                      , Seq(
                          Match(Var("x_45"))
                        , Seq(
                            Build(Var("u_45"))
                          , Seq(
                              CallT(SVar("l_26"), [], [])
                            , Seq(
                                Match(Var("y_45"))
                              , Build(
                                  Anno(
                                    Op(
                                      "OpApp"
                                    , [Var("w_45"), Var("x_45"), Var("y_45")]
                                    )
                                  , Var("v_45")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "AppBin_2_0"
        , [ VarDec(
              "m_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_46", "z_45", "a_46", "c_46", "d_46"]
          , Seq(
              Match(
                Anno(
                  Op("AppBin", [Var("z_45"), Var("a_46")])
                , Var("b_46")
                )
              )
            , Seq(
                Build(Var("z_45"))
              , Seq(
                  CallT(SVar("m_26"), [], [])
                , Seq(
                    Match(Var("c_46"))
                  , Seq(
                      Build(Var("a_46"))
                    , Seq(
                        CallT(SVar("n_26"), [], [])
                      , Seq(
                          Match(Var("d_46"))
                        , Build(
                            Anno(
                              Op("AppBin", [Var("c_46"), Var("d_46")])
                            , Var("b_46")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Case_2_0"
        , [ VarDec(
              "o_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_46", "e_46", "f_46", "h_46", "i_46"]
          , Seq(
              Match(
                Anno(
                  Op("Case", [Var("e_46"), Var("f_46")])
                , Var("g_46")
                )
              )
            , Seq(
                Build(Var("e_46"))
              , Seq(
                  CallT(SVar("o_26"), [], [])
                , Seq(
                    Match(Var("h_46"))
                  , Seq(
                      Build(Var("f_46"))
                    , Seq(
                        CallT(SVar("p_26"), [], [])
                      , Seq(
                          Match(Var("i_46"))
                        , Build(
                            Anno(
                              Op("Case", [Var("h_46"), Var("i_46")])
                            , Var("g_46")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Do_1_0"
        , [ VarDec(
              "q_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_46", "j_46", "l_46"]
          , Seq(
              Match(
                Anno(Op("Do", [Var("j_46")]), Var("k_46"))
              )
            , Seq(
                Build(Var("j_46"))
              , Seq(
                  CallT(SVar("q_26"), [], [])
                , Seq(
                    Match(Var("l_46"))
                  , Build(
                      Anno(Op("Do", [Var("l_46")]), Var("k_46"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "If_3_0"
        , [ VarDec(
              "r_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "s_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_46", "m_46", "n_46", "o_46", "q_46", "r_46", "s_46"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "If"
                  , [Var("m_46"), Var("n_46"), Var("o_46")]
                  )
                , Var("p_46")
                )
              )
            , Seq(
                Build(Var("m_46"))
              , Seq(
                  CallT(SVar("r_26"), [], [])
                , Seq(
                    Match(Var("q_46"))
                  , Seq(
                      Build(Var("n_46"))
                    , Seq(
                        CallT(SVar("s_26"), [], [])
                      , Seq(
                          Match(Var("r_46"))
                        , Seq(
                            Build(Var("o_46"))
                          , Seq(
                              CallT(SVar("t_26"), [], [])
                            , Seq(
                                Match(Var("s_46"))
                              , Build(
                                  Anno(
                                    Op(
                                      "If"
                                    , [Var("q_46"), Var("r_46"), Var("s_46")]
                                    )
                                  , Var("p_46")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Let_2_0"
        , [ VarDec(
              "u_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_46", "t_46", "u_46", "w_46", "x_46"]
          , Seq(
              Match(
                Anno(
                  Op("Let", [Var("t_46"), Var("u_46")])
                , Var("v_46")
                )
              )
            , Seq(
                Build(Var("t_46"))
              , Seq(
                  CallT(SVar("u_26"), [], [])
                , Seq(
                    Match(Var("w_46"))
                  , Seq(
                      Build(Var("u_46"))
                    , Seq(
                        CallT(SVar("v_26"), [], [])
                      , Seq(
                          Match(Var("x_46"))
                        , Build(
                            Anno(
                              Op("Let", [Var("w_46"), Var("x_46")])
                            , Var("v_46")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Abs_2_0"
        , [ VarDec(
              "w_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_47", "y_46", "z_46", "b_47", "c_47"]
          , Seq(
              Match(
                Anno(
                  Op("Abs", [Var("y_46"), Var("z_46")])
                , Var("a_47")
                )
              )
            , Seq(
                Build(Var("y_46"))
              , Seq(
                  CallT(SVar("w_26"), [], [])
                , Seq(
                    Match(Var("b_47"))
                  , Seq(
                      Build(Var("z_46"))
                    , Seq(
                        CallT(SVar("x_26"), [], [])
                      , Seq(
                          Match(Var("c_47"))
                        , Build(
                            Anno(
                              Op("Abs", [Var("b_47"), Var("c_47")])
                            , Var("a_47")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "RSection_2_0"
        , [ VarDec(
              "y_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_47", "d_47", "e_47", "g_47", "h_47"]
          , Seq(
              Match(
                Anno(
                  Op("RSection", [Var("d_47"), Var("e_47")])
                , Var("f_47")
                )
              )
            , Seq(
                Build(Var("d_47"))
              , Seq(
                  CallT(SVar("y_26"), [], [])
                , Seq(
                    Match(Var("g_47"))
                  , Seq(
                      Build(Var("e_47"))
                    , Seq(
                        CallT(SVar("z_26"), [], [])
                      , Seq(
                          Match(Var("h_47"))
                        , Build(
                            Anno(
                              Op("RSection", [Var("g_47"), Var("h_47")])
                            , Var("f_47")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "LSection_2_0"
        , [ VarDec(
              "a_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_47", "i_47", "j_47", "l_47", "m_47"]
          , Seq(
              Match(
                Anno(
                  Op("LSection", [Var("i_47"), Var("j_47")])
                , Var("k_47")
                )
              )
            , Seq(
                Build(Var("i_47"))
              , Seq(
                  CallT(SVar("a_27"), [], [])
                , Seq(
                    Match(Var("l_47"))
                  , Seq(
                      Build(Var("j_47"))
                    , Seq(
                        CallT(SVar("b_27"), [], [])
                      , Seq(
                          Match(Var("m_47"))
                        , Build(
                            Anno(
                              Op("LSection", [Var("l_47"), Var("m_47")])
                            , Var("k_47")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Product_1_0"
        , [ VarDec(
              "c_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_47", "n_47", "p_47"]
          , Seq(
              Match(
                Anno(Op("Product", [Var("n_47")]), Var("o_47"))
              )
            , Seq(
                Build(Var("n_47"))
              , Seq(
                  CallT(SVar("c_27"), [], [])
                , Seq(
                    Match(Var("p_47"))
                  , Build(
                      Anno(Op("Product", [Var("p_47")]), Var("o_47"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Lit_1_0"
        , [ VarDec(
              "d_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_47", "q_47", "s_47"]
          , Seq(
              Match(
                Anno(Op("Lit", [Var("q_47")]), Var("r_47"))
              )
            , Seq(
                Build(Var("q_47"))
              , Seq(
                  CallT(SVar("d_27"), [], [])
                , Seq(
                    Match(Var("s_47"))
                  , Build(
                      Anno(Op("Lit", [Var("s_47")]), Var("r_47"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Constr_1_0"
        , [ VarDec(
              "e_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_47", "t_47", "v_47"]
          , Seq(
              Match(
                Anno(Op("Constr", [Var("t_47")]), Var("u_47"))
              )
            , Seq(
                Build(Var("t_47"))
              , Seq(
                  CallT(SVar("e_27"), [], [])
                , Seq(
                    Match(Var("v_47"))
                  , Build(
                      Anno(Op("Constr", [Var("v_47")]), Var("u_47"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QVar_1_0"
        , [ VarDec(
              "f_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_47", "w_47", "y_47"]
          , Seq(
              Match(
                Anno(Op("QVar", [Var("w_47")]), Var("x_47"))
              )
            , Seq(
                Build(Var("w_47"))
              , Seq(
                  CallT(SVar("f_27"), [], [])
                , Seq(
                    Match(Var("y_47"))
                  , Build(
                      Anno(Op("QVar", [Var("y_47")]), Var("x_47"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrProcedure_2_0"
        , [ VarDec(
              "g_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "h_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_48", "z_47", "a_48", "c_48", "d_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrProcedure", [Var("z_47"), Var("a_48")])
                , Var("b_48")
                )
              )
            , Seq(
                Build(Var("z_47"))
              , Seq(
                  CallT(SVar("g_27"), [], [])
                , Seq(
                    Match(Var("c_48"))
                  , Seq(
                      Build(Var("a_48"))
                    , Seq(
                        CallT(SVar("h_27"), [], [])
                      , Seq(
                          Match(Var("d_48"))
                        , Build(
                            Anno(
                              Op("ArrProcedure", [Var("c_48"), Var("d_48")])
                            , Var("b_48")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrStmtSeq_2_0"
        , [ VarDec(
              "i_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_48", "e_48", "f_48", "h_48", "i_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrStmtSeq", [Var("e_48"), Var("f_48")])
                , Var("g_48")
                )
              )
            , Seq(
                Build(Var("e_48"))
              , Seq(
                  CallT(SVar("i_27"), [], [])
                , Seq(
                    Match(Var("h_48"))
                  , Seq(
                      Build(Var("f_48"))
                    , Seq(
                        CallT(SVar("j_27"), [], [])
                      , Seq(
                          Match(Var("i_48"))
                        , Build(
                            Anno(
                              Op("ArrStmtSeq", [Var("h_48"), Var("i_48")])
                            , Var("g_48")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrStmtList_1_0"
        , [ VarDec(
              "k_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_48", "j_48", "l_48"]
          , Seq(
              Match(
                Anno(Op("ArrStmtList", [Var("j_48")]), Var("k_48"))
              )
            , Seq(
                Build(Var("j_48"))
              , Seq(
                  CallT(SVar("k_27"), [], [])
                , Seq(
                    Match(Var("l_48"))
                  , Build(
                      Anno(Op("ArrStmtList", [Var("l_48")]), Var("k_48"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrCmdStmt_1_0"
        , [ VarDec(
              "l_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_48", "m_48", "o_48"]
          , Seq(
              Match(
                Anno(Op("ArrCmdStmt", [Var("m_48")]), Var("n_48"))
              )
            , Seq(
                Build(Var("m_48"))
              , Seq(
                  CallT(SVar("l_27"), [], [])
                , Seq(
                    Match(Var("o_48"))
                  , Build(
                      Anno(Op("ArrCmdStmt", [Var("o_48")]), Var("n_48"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrBindStmt_2_0"
        , [ VarDec(
              "m_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_48", "p_48", "q_48", "s_48", "t_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrBindStmt", [Var("p_48"), Var("q_48")])
                , Var("r_48")
                )
              )
            , Seq(
                Build(Var("p_48"))
              , Seq(
                  CallT(SVar("m_27"), [], [])
                , Seq(
                    Match(Var("s_48"))
                  , Seq(
                      Build(Var("q_48"))
                    , Seq(
                        CallT(SVar("n_27"), [], [])
                      , Seq(
                          Match(Var("t_48"))
                        , Build(
                            Anno(
                              Op("ArrBindStmt", [Var("s_48"), Var("t_48")])
                            , Var("r_48")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrLetStmt_1_0"
        , [ VarDec(
              "o_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_48", "u_48", "w_48"]
          , Seq(
              Match(
                Anno(Op("ArrLetStmt", [Var("u_48")]), Var("v_48"))
              )
            , Seq(
                Build(Var("u_48"))
              , Seq(
                  CallT(SVar("o_27"), [], [])
                , Seq(
                    Match(Var("w_48"))
                  , Build(
                      Anno(Op("ArrLetStmt", [Var("w_48")]), Var("v_48"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrAltSeqOff_2_0"
        , [ VarDec(
              "p_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_48", "x_48", "y_48", "a_49", "b_49"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAltSeqOff", [Var("x_48"), Var("y_48")])
                , Var("z_48")
                )
              )
            , Seq(
                Build(Var("x_48"))
              , Seq(
                  CallT(SVar("p_27"), [], [])
                , Seq(
                    Match(Var("a_49"))
                  , Seq(
                      Build(Var("y_48"))
                    , Seq(
                        CallT(SVar("q_27"), [], [])
                      , Seq(
                          Match(Var("b_49"))
                        , Build(
                            Anno(
                              Op("ArrAltSeqOff", [Var("a_49"), Var("b_49")])
                            , Var("z_48")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrAltSeq_2_0"
        , [ VarDec(
              "r_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "s_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_49", "c_49", "d_49", "f_49", "g_49"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAltSeq", [Var("c_49"), Var("d_49")])
                , Var("e_49")
                )
              )
            , Seq(
                Build(Var("c_49"))
              , Seq(
                  CallT(SVar("r_27"), [], [])
                , Seq(
                    Match(Var("f_49"))
                  , Seq(
                      Build(Var("d_49"))
                    , Seq(
                        CallT(SVar("s_27"), [], [])
                      , Seq(
                          Match(Var("g_49"))
                        , Build(
                            Anno(
                              Op("ArrAltSeq", [Var("f_49"), Var("g_49")])
                            , Var("e_49")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrAltList_1_0"
        , [ VarDec(
              "t_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_49", "h_49", "j_49"]
          , Seq(
              Match(
                Anno(Op("ArrAltList", [Var("h_49")]), Var("i_49"))
              )
            , Seq(
                Build(Var("h_49"))
              , Seq(
                  CallT(SVar("t_27"), [], [])
                , Seq(
                    Match(Var("j_49"))
                  , Build(
                      Anno(Op("ArrAltList", [Var("j_49")]), Var("i_49"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ArrAlt_3_0"
        , [ VarDec(
              "u_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_49", "k_49", "l_49", "m_49", "o_49", "p_49", "q_49"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ArrAlt"
                  , [Var("k_49"), Var("l_49"), Var("m_49")]
                  )
                , Var("n_49")
                )
              )
            , Seq(
                Build(Var("k_49"))
              , Seq(
                  CallT(SVar("u_27"), [], [])
                , Seq(
                    Match(Var("o_49"))
                  , Seq(
                      Build(Var("l_49"))
                    , Seq(
                        CallT(SVar("v_27"), [], [])
                      , Seq(
                          Match(Var("p_49"))
                        , Seq(
                            Build(Var("m_49"))
                          , Seq(
                              CallT(SVar("w_27"), [], [])
                            , Seq(
                                Match(Var("q_49"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ArrAlt"
                                    , [Var("o_49"), Var("p_49"), Var("q_49")]
                                    )
                                  , Var("n_49")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "SignDecl_3_0"
        , [ VarDec(
              "x_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_49", "r_49", "s_49", "t_49", "v_49", "w_49", "x_49"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "SignDecl"
                  , [Var("r_49"), Var("s_49"), Var("t_49")]
                  )
                , Var("u_49")
                )
              )
            , Seq(
                Build(Var("r_49"))
              , Seq(
                  CallT(SVar("x_27"), [], [])
                , Seq(
                    Match(Var("v_49"))
                  , Seq(
                      Build(Var("s_49"))
                    , Seq(
                        CallT(SVar("y_27"), [], [])
                      , Seq(
                          Match(Var("w_49"))
                        , Seq(
                            Build(Var("t_49"))
                          , Seq(
                              CallT(SVar("z_27"), [], [])
                            , Seq(
                                Match(Var("x_49"))
                              , Build(
                                  Anno(
                                    Op(
                                      "SignDecl"
                                    , [Var("v_49"), Var("w_49"), Var("x_49")]
                                    )
                                  , Var("u_49")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ClassMulti_3_0"
        , [ VarDec(
              "a_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "c_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_50", "y_49", "z_49", "a_50", "c_50", "d_50", "e_50"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ClassMulti"
                  , [Var("y_49"), Var("z_49"), Var("a_50")]
                  )
                , Var("b_50")
                )
              )
            , Seq(
                Build(Var("y_49"))
              , Seq(
                  CallT(SVar("a_28"), [], [])
                , Seq(
                    Match(Var("c_50"))
                  , Seq(
                      Build(Var("z_49"))
                    , Seq(
                        CallT(SVar("b_28"), [], [])
                      , Seq(
                          Match(Var("d_50"))
                        , Seq(
                            Build(Var("a_50"))
                          , Seq(
                              CallT(SVar("c_28"), [], [])
                            , Seq(
                                Match(Var("e_50"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ClassMulti"
                                    , [Var("c_50"), Var("d_50"), Var("e_50")]
                                    )
                                  , Var("b_50")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "SimpleClass_2_0"
        , [ VarDec(
              "d_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_50", "f_50", "g_50", "i_50", "j_50"]
          , Seq(
              Match(
                Anno(
                  Op("SimpleClass", [Var("f_50"), Var("g_50")])
                , Var("h_50")
                )
              )
            , Seq(
                Build(Var("f_50"))
              , Seq(
                  CallT(SVar("d_28"), [], [])
                , Seq(
                    Match(Var("i_50"))
                  , Seq(
                      Build(Var("g_50"))
                    , Seq(
                        CallT(SVar("e_28"), [], [])
                      , Seq(
                          Match(Var("j_50"))
                        , Build(
                            Anno(
                              Op("SimpleClass", [Var("i_50"), Var("j_50")])
                            , Var("h_50")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "SContext_1_0"
        , [ VarDec(
              "f_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_50", "k_50", "m_50"]
          , Seq(
              Match(
                Anno(Op("SContext", [Var("k_50")]), Var("l_50"))
              )
            , Seq(
                Build(Var("k_50"))
              , Seq(
                  CallT(SVar("f_28"), [], [])
                , Seq(
                    Match(Var("m_50"))
                  , Build(
                      Anno(Op("SContext", [Var("m_50")]), Var("l_50"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Context_1_0"
        , [ VarDec(
              "g_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_50", "n_50", "p_50"]
          , Seq(
              Match(
                Anno(Op("Context", [Var("n_50")]), Var("o_50"))
              )
            , Seq(
                Build(Var("n_50"))
              , Seq(
                  CallT(SVar("g_28"), [], [])
                , Seq(
                    Match(Var("p_50"))
                  , Build(
                      Anno(Op("Context", [Var("p_50")]), Var("o_50"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InstArrow_2_0"
        , [ VarDec(
              "h_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_50", "q_50", "r_50", "t_50", "u_50"]
          , Seq(
              Match(
                Anno(
                  Op("InstArrow", [Var("q_50"), Var("r_50")])
                , Var("s_50")
                )
              )
            , Seq(
                Build(Var("q_50"))
              , Seq(
                  CallT(SVar("h_28"), [], [])
                , Seq(
                    Match(Var("t_50"))
                  , Seq(
                      Build(Var("r_50"))
                    , Seq(
                        CallT(SVar("i_28"), [], [])
                      , Seq(
                          Match(Var("u_50"))
                        , Build(
                            Anno(
                              Op("InstArrow", [Var("t_50"), Var("u_50")])
                            , Var("s_50")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InstList_1_0"
        , [ VarDec(
              "j_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_50", "v_50", "x_50"]
          , Seq(
              Match(
                Anno(Op("InstList", [Var("v_50")]), Var("w_50"))
              )
            , Seq(
                Build(Var("v_50"))
              , Seq(
                  CallT(SVar("j_28"), [], [])
                , Seq(
                    Match(Var("x_50"))
                  , Build(
                      Anno(Op("InstList", [Var("x_50")]), Var("w_50"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InstTuple_2_0"
        , [ VarDec(
              "k_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "l_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_51", "y_50", "z_50", "b_51", "c_51"]
          , Seq(
              Match(
                Anno(
                  Op("InstTuple", [Var("y_50"), Var("z_50")])
                , Var("a_51")
                )
              )
            , Seq(
                Build(Var("y_50"))
              , Seq(
                  CallT(SVar("k_28"), [], [])
                , Seq(
                    Match(Var("b_51"))
                  , Seq(
                      Build(Var("z_50"))
                    , Seq(
                        CallT(SVar("l_28"), [], [])
                      , Seq(
                          Match(Var("c_51"))
                        , Build(
                            Anno(
                              Op("InstTuple", [Var("b_51"), Var("c_51")])
                            , Var("a_51")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InstApp_2_0"
        , [ VarDec(
              "m_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_51", "d_51", "e_51", "g_51", "h_51"]
          , Seq(
              Match(
                Anno(
                  Op("InstApp", [Var("d_51"), Var("e_51")])
                , Var("f_51")
                )
              )
            , Seq(
                Build(Var("d_51"))
              , Seq(
                  CallT(SVar("m_28"), [], [])
                , Seq(
                    Match(Var("g_51"))
                  , Seq(
                      Build(Var("e_51"))
                    , Seq(
                        CallT(SVar("n_28"), [], [])
                      , Seq(
                          Match(Var("h_51"))
                        , Build(
                            Anno(
                              Op("InstApp", [Var("g_51"), Var("h_51")])
                            , Var("f_51")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InstCons_1_0"
        , [ VarDec(
              "o_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_51", "i_51", "k_51"]
          , Seq(
              Match(
                Anno(Op("InstCons", [Var("i_51")]), Var("j_51"))
              )
            , Seq(
                Build(Var("i_51"))
              , Seq(
                  CallT(SVar("o_28"), [], [])
                , Seq(
                    Match(Var("k_51"))
                  , Build(
                      Anno(Op("InstCons", [Var("k_51")]), Var("j_51"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "InfixConstr_3_0"
        , [ VarDec(
              "p_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_51", "l_51", "m_51", "n_51", "p_51", "q_51", "r_51"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "InfixConstr"
                  , [Var("l_51"), Var("m_51"), Var("n_51")]
                  )
                , Var("o_51")
                )
              )
            , Seq(
                Build(Var("l_51"))
              , Seq(
                  CallT(SVar("p_28"), [], [])
                , Seq(
                    Match(Var("p_51"))
                  , Seq(
                      Build(Var("m_51"))
                    , Seq(
                        CallT(SVar("q_28"), [], [])
                      , Seq(
                          Match(Var("q_51"))
                        , Seq(
                            Build(Var("n_51"))
                          , Seq(
                              CallT(SVar("r_28"), [], [])
                            , Seq(
                                Match(Var("r_51"))
                              , Build(
                                  Anno(
                                    Op(
                                      "InfixConstr"
                                    , [Var("p_51"), Var("q_51"), Var("r_51")]
                                    )
                                  , Var("o_51")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ConstrDecl_2_0"
        , [ VarDec(
              "s_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_51", "s_51", "t_51", "v_51", "w_51"]
          , Seq(
              Match(
                Anno(
                  Op("ConstrDecl", [Var("s_51"), Var("t_51")])
                , Var("u_51")
                )
              )
            , Seq(
                Build(Var("s_51"))
              , Seq(
                  CallT(SVar("s_28"), [], [])
                , Seq(
                    Match(Var("v_51"))
                  , Seq(
                      Build(Var("t_51"))
                    , Seq(
                        CallT(SVar("t_28"), [], [])
                      , Seq(
                          Match(Var("w_51"))
                        , Build(
                            Anno(
                              Op("ConstrDecl", [Var("v_51"), Var("w_51")])
                            , Var("u_51")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ConstrDecls_1_0"
        , [ VarDec(
              "u_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_51", "x_51", "z_51"]
          , Seq(
              Match(
                Anno(Op("ConstrDecls", [Var("x_51")]), Var("y_51"))
              )
            , Seq(
                Build(Var("x_51"))
              , Seq(
                  CallT(SVar("u_28"), [], [])
                , Seq(
                    Match(Var("z_51"))
                  , Build(
                      Anno(Op("ConstrDecls", [Var("z_51")]), Var("y_51"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "NoConstrDecls_0_0"
        , []
        , []
        , Match(Anno(Op("NoConstrDecls", []), Wld()))
        )
      , SDefT(
          "Derive_1_0"
        , [ VarDec(
              "v_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_52", "a_52", "c_52"]
          , Seq(
              Match(
                Anno(Op("Derive", [Var("a_52")]), Var("b_52"))
              )
            , Seq(
                Build(Var("a_52"))
              , Seq(
                  CallT(SVar("v_28"), [], [])
                , Seq(
                    Match(Var("c_52"))
                  , Build(
                      Anno(Op("Derive", [Var("c_52")]), Var("b_52"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "NoDeriving_0_0"
        , []
        , []
        , Match(Anno(Op("NoDeriving", []), Wld()))
        )
      , SDefT(
          "TFunBin_2_0"
        , [ VarDec(
              "w_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_52", "d_52", "e_52", "g_52", "h_52"]
          , Seq(
              Match(
                Anno(
                  Op("TFunBin", [Var("d_52"), Var("e_52")])
                , Var("f_52")
                )
              )
            , Seq(
                Build(Var("d_52"))
              , Seq(
                  CallT(SVar("w_28"), [], [])
                , Seq(
                    Match(Var("g_52"))
                  , Seq(
                      Build(Var("e_52"))
                    , Seq(
                        CallT(SVar("x_28"), [], [])
                      , Seq(
                          Match(Var("h_52"))
                        , Build(
                            Anno(
                              Op("TFunBin", [Var("g_52"), Var("h_52")])
                            , Var("f_52")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TAppBin_2_0"
        , [ VarDec(
              "y_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_52", "i_52", "j_52", "l_52", "m_52"]
          , Seq(
              Match(
                Anno(
                  Op("TAppBin", [Var("i_52"), Var("j_52")])
                , Var("k_52")
                )
              )
            , Seq(
                Build(Var("i_52"))
              , Seq(
                  CallT(SVar("y_28"), [], [])
                , Seq(
                    Match(Var("l_52"))
                  , Seq(
                      Build(Var("j_52"))
                    , Seq(
                        CallT(SVar("z_28"), [], [])
                      , Seq(
                          Match(Var("m_52"))
                        , Build(
                            Anno(
                              Op("TAppBin", [Var("l_52"), Var("m_52")])
                            , Var("k_52")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TProd_1_0"
        , [ VarDec(
              "a_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_52", "n_52", "p_52"]
          , Seq(
              Match(
                Anno(Op("TProd", [Var("n_52")]), Var("o_52"))
              )
            , Seq(
                Build(Var("n_52"))
              , Seq(
                  CallT(SVar("a_29"), [], [])
                , Seq(
                    Match(Var("p_52"))
                  , Build(
                      Anno(Op("TProd", [Var("p_52")]), Var("o_52"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TList_1_0"
        , [ VarDec(
              "b_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_52", "q_52", "s_52"]
          , Seq(
              Match(
                Anno(Op("TList", [Var("q_52")]), Var("r_52"))
              )
            , Seq(
                Build(Var("q_52"))
              , Seq(
                  CallT(SVar("b_29"), [], [])
                , Seq(
                    Match(Var("s_52"))
                  , Build(
                      Anno(Op("TList", [Var("s_52")]), Var("r_52"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TVar_1_0"
        , [ VarDec(
              "c_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_52", "t_52", "v_52"]
          , Seq(
              Match(
                Anno(Op("TVar", [Var("t_52")]), Var("u_52"))
              )
            , Seq(
                Build(Var("t_52"))
              , Seq(
                  CallT(SVar("c_29"), [], [])
                , Seq(
                    Match(Var("v_52"))
                  , Build(
                      Anno(Op("TVar", [Var("v_52")]), Var("u_52"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TCon_1_0"
        , [ VarDec(
              "d_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_52", "w_52", "y_52"]
          , Seq(
              Match(
                Anno(Op("TCon", [Var("w_52")]), Var("x_52"))
              )
            , Seq(
                Build(Var("w_52"))
              , Seq(
                  CallT(SVar("d_29"), [], [])
                , Seq(
                    Match(Var("y_52"))
                  , Build(
                      Anno(Op("TCon", [Var("y_52")]), Var("x_52"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TCons_2_0"
        , [ VarDec(
              "e_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_53", "z_52", "a_53", "c_53", "d_53"]
          , Seq(
              Match(
                Anno(
                  Op("TCons", [Var("z_52"), Var("a_53")])
                , Var("b_53")
                )
              )
            , Seq(
                Build(Var("z_52"))
              , Seq(
                  CallT(SVar("e_29"), [], [])
                , Seq(
                    Match(Var("c_53"))
                  , Seq(
                      Build(Var("a_53"))
                    , Seq(
                        CallT(SVar("f_29"), [], [])
                      , Seq(
                          Match(Var("d_53"))
                        , Build(
                            Anno(
                              Op("TCons", [Var("c_53"), Var("d_53")])
                            , Var("b_53")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TListCon_0_0"
        , []
        , []
        , Match(Anno(Op("TListCon", []), Wld()))
        )
      , SDefT(
          "TUnit_0_0"
        , []
        , []
        , Match(Anno(Op("TUnit", []), Wld()))
        )
      , SDefT(
          "TArrow_0_0"
        , []
        , []
        , Match(Anno(Op("TArrow", []), Wld()))
        )
      , SDefT(
          "Hiding_1_0"
        , [ VarDec(
              "g_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_53", "e_53", "g_53"]
          , Seq(
              Match(
                Anno(Op("Hiding", [Var("e_53")]), Var("f_53"))
              )
            , Seq(
                Build(Var("e_53"))
              , Seq(
                  CallT(SVar("g_29"), [], [])
                , Seq(
                    Match(Var("g_53"))
                  , Build(
                      Anno(Op("Hiding", [Var("g_53")]), Var("f_53"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Impspec_1_0"
        , [ VarDec(
              "h_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_53", "h_53", "j_53"]
          , Seq(
              Match(
                Anno(Op("Impspec", [Var("h_53")]), Var("i_53"))
              )
            , Seq(
                Build(Var("h_53"))
              , Seq(
                  CallT(SVar("h_29"), [], [])
                , Seq(
                    Match(Var("j_53"))
                  , Build(
                      Anno(Op("Impspec", [Var("j_53")]), Var("i_53"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "As_1_0"
        , [ VarDec(
              "i_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_53", "k_53", "m_53"]
          , Seq(
              Match(
                Anno(Op("As", [Var("k_53")]), Var("l_53"))
              )
            , Seq(
                Build(Var("k_53"))
              , Seq(
                  CallT(SVar("i_29"), [], [])
                , Seq(
                    Match(Var("m_53"))
                  , Build(
                      Anno(Op("As", [Var("m_53")]), Var("l_53"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Qualified_0_0"
        , []
        , []
        , Match(Anno(Op("Qualified", []), Wld()))
        )
      , SDefT(
          "SOURCE_0_0"
        , []
        , []
        , Match(Anno(Op("SOURCE", []), Wld()))
        )
      , SDefT(
          "Import_5_0"
        , [ VarDec(
              "j_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "l_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            [ "s_53"
            , "n_53"
            , "o_53"
            , "p_53"
            , "q_53"
            , "r_53"
            , "t_53"
            , "u_53"
            , "v_53"
            , "w_53"
            , "x_53"
            ]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Import"
                  , [ Var("n_53")
                    , Var("o_53")
                    , Var("p_53")
                    , Var("q_53")
                    , Var("r_53")
                    ]
                  )
                , Var("s_53")
                )
              )
            , Seq(
                Build(Var("n_53"))
              , Seq(
                  CallT(SVar("j_29"), [], [])
                , Seq(
                    Match(Var("t_53"))
                  , Seq(
                      Build(Var("o_53"))
                    , Seq(
                        CallT(SVar("k_29"), [], [])
                      , Seq(
                          Match(Var("u_53"))
                        , Seq(
                            Build(Var("p_53"))
                          , Seq(
                              CallT(SVar("l_29"), [], [])
                            , Seq(
                                Match(Var("v_53"))
                              , Seq(
                                  Build(Var("q_53"))
                                , Seq(
                                    CallT(SVar("m_29"), [], [])
                                  , Seq(
                                      Match(Var("w_53"))
                                    , Seq(
                                        Build(Var("r_53"))
                                      , Seq(
                                          CallT(SVar("n_29"), [], [])
                                        , Seq(
                                            Match(Var("x_53"))
                                          , Build(
                                              Anno(
                                                Op(
                                                  "Import"
                                                , [ Var("t_53")
                                                  , Var("u_53")
                                                  , Var("v_53")
                                                  , Var("w_53")
                                                  , Var("x_53")
                                                  ]
                                                )
                                              , Var("s_53")
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Exports_1_0"
        , [ VarDec(
              "o_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_53", "y_53", "a_54"]
          , Seq(
              Match(
                Anno(Op("Exports", [Var("y_53")]), Var("z_53"))
              )
            , Seq(
                Build(Var("y_53"))
              , Seq(
                  CallT(SVar("o_29"), [], [])
                , Seq(
                    Match(Var("a_54"))
                  , Build(
                      Anno(Op("Exports", [Var("a_54")]), Var("z_53"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Exportlist_1_0"
        , [ VarDec(
              "p_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_54", "b_54", "d_54"]
          , Seq(
              Match(
                Anno(Op("Exportlist", [Var("b_54")]), Var("c_54"))
              )
            , Seq(
                Build(Var("b_54"))
              , Seq(
                  CallT(SVar("p_29"), [], [])
                , Seq(
                    Match(Var("d_54"))
                  , Build(
                      Anno(Op("Exportlist", [Var("d_54")]), Var("c_54"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TopdeclSeq_2_0"
        , [ VarDec(
              "q_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_54", "e_54", "f_54", "h_54", "i_54"]
          , Seq(
              Match(
                Anno(
                  Op("TopdeclSeq", [Var("e_54"), Var("f_54")])
                , Var("g_54")
                )
              )
            , Seq(
                Build(Var("e_54"))
              , Seq(
                  CallT(SVar("q_29"), [], [])
                , Seq(
                    Match(Var("h_54"))
                  , Seq(
                      Build(Var("f_54"))
                    , Seq(
                        CallT(SVar("r_29"), [], [])
                      , Seq(
                          Match(Var("i_54"))
                        , Build(
                            Anno(
                              Op("TopdeclSeq", [Var("h_54"), Var("i_54")])
                            , Var("g_54")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ImportdeclSeq_2_0"
        , [ VarDec(
              "s_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_54", "j_54", "k_54", "m_54", "n_54"]
          , Seq(
              Match(
                Anno(
                  Op("ImportdeclSeq", [Var("j_54"), Var("k_54")])
                , Var("l_54")
                )
              )
            , Seq(
                Build(Var("j_54"))
              , Seq(
                  CallT(SVar("s_29"), [], [])
                , Seq(
                    Match(Var("m_54"))
                  , Seq(
                      Build(Var("k_54"))
                    , Seq(
                        CallT(SVar("t_29"), [], [])
                      , Seq(
                          Match(Var("n_54"))
                        , Build(
                            Anno(
                              Op("ImportdeclSeq", [Var("m_54"), Var("n_54")])
                            , Var("l_54")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "OffBody_2_0"
        , [ VarDec(
              "u_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_54", "o_54", "p_54", "r_54", "s_54"]
          , Seq(
              Match(
                Anno(
                  Op("OffBody", [Var("o_54"), Var("p_54")])
                , Var("q_54")
                )
              )
            , Seq(
                Build(Var("o_54"))
              , Seq(
                  CallT(SVar("u_29"), [], [])
                , Seq(
                    Match(Var("r_54"))
                  , Seq(
                      Build(Var("p_54"))
                    , Seq(
                        CallT(SVar("v_29"), [], [])
                      , Seq(
                          Match(Var("s_54"))
                        , Build(
                            Anno(
                              Op("OffBody", [Var("r_54"), Var("s_54")])
                            , Var("q_54")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Body_2_0"
        , [ VarDec(
              "w_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_54", "t_54", "u_54", "w_54", "x_54"]
          , Seq(
              Match(
                Anno(
                  Op("Body", [Var("t_54"), Var("u_54")])
                , Var("v_54")
                )
              )
            , Seq(
                Build(Var("t_54"))
              , Seq(
                  CallT(SVar("w_29"), [], [])
                , Seq(
                    Match(Var("w_54"))
                  , Seq(
                      Build(Var("u_54"))
                    , Seq(
                        CallT(SVar("x_29"), [], [])
                      , Seq(
                          Match(Var("x_54"))
                        , Build(
                            Anno(
                              Op("Body", [Var("w_54"), Var("x_54")])
                            , Var("v_54")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Empty_0_0"
        , []
        , []
        , Match(Anno(Op("Empty", []), Wld()))
        )
      , SDefT(
          "FlexibleInstance_4_0"
        , [ VarDec(
              "y_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "a_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_55", "y_54", "z_54", "a_55", "b_55", "d_55", "e_55", "f_55", "g_55"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "FlexibleInstance"
                  , [Var("y_54"), Var("z_54"), Var("a_55"), Var("b_55")]
                  )
                , Var("c_55")
                )
              )
            , Seq(
                Build(Var("y_54"))
              , Seq(
                  CallT(SVar("y_29"), [], [])
                , Seq(
                    Match(Var("d_55"))
                  , Seq(
                      Build(Var("z_54"))
                    , Seq(
                        CallT(SVar("z_29"), [], [])
                      , Seq(
                          Match(Var("e_55"))
                        , Seq(
                            Build(Var("a_55"))
                          , Seq(
                              CallT(SVar("a_30"), [], [])
                            , Seq(
                                Match(Var("f_55"))
                              , Seq(
                                  Build(Var("b_55"))
                                , Seq(
                                    CallT(SVar("b_30"), [], [])
                                  , Seq(
                                      Match(Var("g_55"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "FlexibleInstance"
                                          , [Var("d_55"), Var("e_55"), Var("f_55"), Var("g_55")]
                                          )
                                        , Var("c_55")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Default_1_0"
        , [ VarDec(
              "c_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_55", "h_55", "j_55"]
          , Seq(
              Match(
                Anno(Op("Default", [Var("h_55")]), Var("i_55"))
              )
            , Seq(
                Build(Var("h_55"))
              , Seq(
                  CallT(SVar("c_30"), [], [])
                , Seq(
                    Match(Var("j_55"))
                  , Build(
                      Anno(Op("Default", [Var("j_55")]), Var("i_55"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Instance_4_0"
        , [ VarDec(
              "d_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_55", "k_55", "l_55", "m_55", "n_55", "p_55", "q_55", "r_55", "s_55"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Instance"
                  , [Var("k_55"), Var("l_55"), Var("m_55"), Var("n_55")]
                  )
                , Var("o_55")
                )
              )
            , Seq(
                Build(Var("k_55"))
              , Seq(
                  CallT(SVar("d_30"), [], [])
                , Seq(
                    Match(Var("p_55"))
                  , Seq(
                      Build(Var("l_55"))
                    , Seq(
                        CallT(SVar("e_30"), [], [])
                      , Seq(
                          Match(Var("q_55"))
                        , Seq(
                            Build(Var("m_55"))
                          , Seq(
                              CallT(SVar("f_30"), [], [])
                            , Seq(
                                Match(Var("r_55"))
                              , Seq(
                                  Build(Var("n_55"))
                                , Seq(
                                    CallT(SVar("g_30"), [], [])
                                  , Seq(
                                      Match(Var("s_55"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "Instance"
                                          , [Var("p_55"), Var("q_55"), Var("r_55"), Var("s_55")]
                                          )
                                        , Var("o_55")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Class_4_0"
        , [ VarDec(
              "h_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_55", "t_55", "u_55", "v_55", "w_55", "y_55", "z_55", "a_56", "b_56"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Class"
                  , [Var("t_55"), Var("u_55"), Var("v_55"), Var("w_55")]
                  )
                , Var("x_55")
                )
              )
            , Seq(
                Build(Var("t_55"))
              , Seq(
                  CallT(SVar("h_30"), [], [])
                , Seq(
                    Match(Var("y_55"))
                  , Seq(
                      Build(Var("u_55"))
                    , Seq(
                        CallT(SVar("i_30"), [], [])
                      , Seq(
                          Match(Var("z_55"))
                        , Seq(
                            Build(Var("v_55"))
                          , Seq(
                              CallT(SVar("j_30"), [], [])
                            , Seq(
                                Match(Var("a_56"))
                              , Seq(
                                  Build(Var("w_55"))
                                , Seq(
                                    CallT(SVar("k_30"), [], [])
                                  , Seq(
                                      Match(Var("b_56"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "Class"
                                          , [Var("y_55"), Var("z_55"), Var("a_56"), Var("b_56")]
                                          )
                                        , Var("x_55")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Data_4_0"
        , [ VarDec(
              "l_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_56", "c_56", "d_56", "e_56", "f_56", "h_56", "i_56", "j_56", "k_56"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Data"
                  , [Var("c_56"), Var("d_56"), Var("e_56"), Var("f_56")]
                  )
                , Var("g_56")
                )
              )
            , Seq(
                Build(Var("c_56"))
              , Seq(
                  CallT(SVar("l_30"), [], [])
                , Seq(
                    Match(Var("h_56"))
                  , Seq(
                      Build(Var("d_56"))
                    , Seq(
                        CallT(SVar("m_30"), [], [])
                      , Seq(
                          Match(Var("i_56"))
                        , Seq(
                            Build(Var("e_56"))
                          , Seq(
                              CallT(SVar("n_30"), [], [])
                            , Seq(
                                Match(Var("j_56"))
                              , Seq(
                                  Build(Var("f_56"))
                                , Seq(
                                    CallT(SVar("o_30"), [], [])
                                  , Seq(
                                      Match(Var("k_56"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "Data"
                                          , [Var("h_56"), Var("i_56"), Var("j_56"), Var("k_56")]
                                          )
                                        , Var("g_56")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "TypeDecl_3_0"
        , [ VarDec(
              "p_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_56", "l_56", "m_56", "n_56", "p_56", "q_56", "r_56"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "TypeDecl"
                  , [Var("l_56"), Var("m_56"), Var("n_56")]
                  )
                , Var("o_56")
                )
              )
            , Seq(
                Build(Var("l_56"))
              , Seq(
                  CallT(SVar("p_30"), [], [])
                , Seq(
                    Match(Var("p_56"))
                  , Seq(
                      Build(Var("m_56"))
                    , Seq(
                        CallT(SVar("q_30"), [], [])
                      , Seq(
                          Match(Var("q_56"))
                        , Seq(
                            Build(Var("n_56"))
                          , Seq(
                              CallT(SVar("r_30"), [], [])
                            , Seq(
                                Match(Var("r_56"))
                              , Build(
                                  Anno(
                                    Op(
                                      "TypeDecl"
                                    , [Var("p_56"), Var("q_56"), Var("r_56")]
                                    )
                                  , Var("o_56")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Program_1_0"
        , [ VarDec(
              "s_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_56", "s_56", "u_56"]
          , Seq(
              Match(
                Anno(Op("Program", [Var("s_56")]), Var("t_56"))
              )
            , Seq(
                Build(Var("s_56"))
              , Seq(
                  CallT(SVar("s_30"), [], [])
                , Seq(
                    Match(Var("u_56"))
                  , Build(
                      Anno(Op("Program", [Var("u_56")]), Var("t_56"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Module_2_0"
        , [ VarDec(
              "t_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_56", "v_56", "w_56", "y_56", "z_56"]
          , Seq(
              Match(
                Anno(
                  Op("Module", [Var("v_56"), Var("w_56")])
                , Var("x_56")
                )
              )
            , Seq(
                Build(Var("v_56"))
              , Seq(
                  CallT(SVar("t_30"), [], [])
                , Seq(
                    Match(Var("y_56"))
                  , Seq(
                      Build(Var("w_56"))
                    , Seq(
                        CallT(SVar("u_30"), [], [])
                      , Seq(
                          Match(Var("z_56"))
                        , Build(
                            Anno(
                              Op("Module", [Var("y_56"), Var("z_56")])
                            , Var("x_56")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ModuleDec_2_0"
        , [ VarDec(
              "v_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_57", "a_57", "b_57", "d_57", "e_57"]
          , Seq(
              Match(
                Anno(
                  Op("ModuleDec", [Var("a_57"), Var("b_57")])
                , Var("c_57")
                )
              )
            , Seq(
                Build(Var("a_57"))
              , Seq(
                  CallT(SVar("v_30"), [], [])
                , Seq(
                    Match(Var("d_57"))
                  , Seq(
                      Build(Var("b_57"))
                    , Seq(
                        CallT(SVar("w_30"), [], [])
                      , Seq(
                          Match(Var("e_57"))
                        , Build(
                            Anno(
                              Op("ModuleDec", [Var("d_57"), Var("e_57")])
                            , Var("c_57")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "CLitLit_1_0"
        , [ VarDec(
              "x_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_57", "f_57", "h_57"]
          , Seq(
              Match(
                Anno(Op("CLitLit", [Var("f_57")]), Var("g_57"))
              )
            , Seq(
                Build(Var("f_57"))
              , Seq(
                  CallT(SVar("x_30"), [], [])
                , Seq(
                    Match(Var("h_57"))
                  , Build(
                      Anno(Op("CLitLit", [Var("h_57")]), Var("g_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrimDouble_1_0"
        , [ VarDec(
              "y_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_57", "i_57", "k_57"]
          , Seq(
              Match(
                Anno(Op("PrimDouble", [Var("i_57")]), Var("j_57"))
              )
            , Seq(
                Build(Var("i_57"))
              , Seq(
                  CallT(SVar("y_30"), [], [])
                , Seq(
                    Match(Var("k_57"))
                  , Build(
                      Anno(Op("PrimDouble", [Var("k_57")]), Var("j_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrimFloat_1_0"
        , [ VarDec(
              "z_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_57", "l_57", "n_57"]
          , Seq(
              Match(
                Anno(Op("PrimFloat", [Var("l_57")]), Var("m_57"))
              )
            , Seq(
                Build(Var("l_57"))
              , Seq(
                  CallT(SVar("z_30"), [], [])
                , Seq(
                    Match(Var("n_57"))
                  , Build(
                      Anno(Op("PrimFloat", [Var("n_57")]), Var("m_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrimString_1_0"
        , [ VarDec(
              "a_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_57", "o_57", "q_57"]
          , Seq(
              Match(
                Anno(Op("PrimString", [Var("o_57")]), Var("p_57"))
              )
            , Seq(
                Build(Var("o_57"))
              , Seq(
                  CallT(SVar("a_31"), [], [])
                , Seq(
                    Match(Var("q_57"))
                  , Build(
                      Anno(Op("PrimString", [Var("q_57")]), Var("p_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrimChar_1_0"
        , [ VarDec(
              "b_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_57", "r_57", "t_57"]
          , Seq(
              Match(
                Anno(Op("PrimChar", [Var("r_57")]), Var("s_57"))
              )
            , Seq(
                Build(Var("r_57"))
              , Seq(
                  CallT(SVar("b_31"), [], [])
                , Seq(
                    Match(Var("t_57"))
                  , Build(
                      Anno(Op("PrimChar", [Var("t_57")]), Var("s_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrimInt_1_0"
        , [ VarDec(
              "c_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_57", "u_57", "w_57"]
          , Seq(
              Match(
                Anno(Op("PrimInt", [Var("u_57")]), Var("v_57"))
              )
            , Seq(
                Build(Var("u_57"))
              , Seq(
                  CallT(SVar("c_31"), [], [])
                , Seq(
                    Match(Var("w_57"))
                  , Build(
                      Anno(Op("PrimInt", [Var("w_57")]), Var("v_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Float_1_0"
        , [ VarDec(
              "d_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_57", "x_57", "z_57"]
          , Seq(
              Match(
                Anno(Op("Float", [Var("x_57")]), Var("y_57"))
              )
            , Seq(
                Build(Var("x_57"))
              , Seq(
                  CallT(SVar("d_31"), [], [])
                , Seq(
                    Match(Var("z_57"))
                  , Build(
                      Anno(Op("Float", [Var("z_57")]), Var("y_57"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Int_1_0"
        , [ VarDec(
              "e_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_58", "a_58", "c_58"]
          , Seq(
              Match(
                Anno(Op("Int", [Var("a_58")]), Var("b_58"))
              )
            , Seq(
                Build(Var("a_58"))
              , Seq(
                  CallT(SVar("e_31"), [], [])
                , Seq(
                    Match(Var("c_58"))
                  , Build(
                      Anno(Op("Int", [Var("c_58")]), Var("b_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "HexadecimalEsc_1_0"
        , [ VarDec(
              "f_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_58", "d_58", "f_58"]
          , Seq(
              Match(
                Anno(Op("HexadecimalEsc", [Var("d_58")]), Var("e_58"))
              )
            , Seq(
                Build(Var("d_58"))
              , Seq(
                  CallT(SVar("f_31"), [], [])
                , Seq(
                    Match(Var("f_58"))
                  , Build(
                      Anno(Op("HexadecimalEsc", [Var("f_58")]), Var("e_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "OctalEsc_1_0"
        , [ VarDec(
              "g_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_58", "g_58", "i_58"]
          , Seq(
              Match(
                Anno(Op("OctalEsc", [Var("g_58")]), Var("h_58"))
              )
            , Seq(
                Build(Var("g_58"))
              , Seq(
                  CallT(SVar("g_31"), [], [])
                , Seq(
                    Match(Var("i_58"))
                  , Build(
                      Anno(Op("OctalEsc", [Var("i_58")]), Var("h_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DecimalEsc_1_0"
        , [ VarDec(
              "h_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_58", "j_58", "l_58"]
          , Seq(
              Match(
                Anno(Op("DecimalEsc", [Var("j_58")]), Var("k_58"))
              )
            , Seq(
                Build(Var("j_58"))
              , Seq(
                  CallT(SVar("h_31"), [], [])
                , Seq(
                    Match(Var("l_58"))
                  , Build(
                      Anno(Op("DecimalEsc", [Var("l_58")]), Var("k_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ASCIIEsc_1_0"
        , [ VarDec(
              "i_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_58", "m_58", "o_58"]
          , Seq(
              Match(
                Anno(Op("ASCIIEsc", [Var("m_58")]), Var("n_58"))
              )
            , Seq(
                Build(Var("m_58"))
              , Seq(
                  CallT(SVar("i_31"), [], [])
                , Seq(
                    Match(Var("o_58"))
                  , Build(
                      Anno(Op("ASCIIEsc", [Var("o_58")]), Var("n_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "CharEsc_1_0"
        , [ VarDec(
              "j_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_58", "p_58", "r_58"]
          , Seq(
              Match(
                Anno(Op("CharEsc", [Var("p_58")]), Var("q_58"))
              )
            , Seq(
                Build(Var("p_58"))
              , Seq(
                  CallT(SVar("j_31"), [], [])
                , Seq(
                    Match(Var("r_58"))
                  , Build(
                      Anno(Op("CharEsc", [Var("r_58")]), Var("q_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Gap_1_0"
        , [ VarDec(
              "k_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_58", "s_58", "u_58"]
          , Seq(
              Match(
                Anno(Op("Gap", [Var("s_58")]), Var("t_58"))
              )
            , Seq(
                Build(Var("s_58"))
              , Seq(
                  CallT(SVar("k_31"), [], [])
                , Seq(
                    Match(Var("u_58"))
                  , Build(
                      Anno(Op("Gap", [Var("u_58")]), Var("t_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "EscapeString_1_0"
        , [ VarDec(
              "l_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_58", "v_58", "x_58"]
          , Seq(
              Match(
                Anno(Op("EscapeString", [Var("v_58")]), Var("w_58"))
              )
            , Seq(
                Build(Var("v_58"))
              , Seq(
                  CallT(SVar("l_31"), [], [])
                , Seq(
                    Match(Var("x_58"))
                  , Build(
                      Anno(Op("EscapeString", [Var("x_58")]), Var("w_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Escape_1_0"
        , [ VarDec(
              "m_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_58", "y_58", "a_59"]
          , Seq(
              Match(
                Anno(Op("Escape", [Var("y_58")]), Var("z_58"))
              )
            , Seq(
                Build(Var("y_58"))
              , Seq(
                  CallT(SVar("m_31"), [], [])
                , Seq(
                    Match(Var("a_59"))
                  , Build(
                      Anno(Op("Escape", [Var("a_59")]), Var("z_58"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "String_1_0"
        , [ VarDec(
              "n_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_59", "b_59", "d_59"]
          , Seq(
              Match(
                Anno(Op("String", [Var("b_59")]), Var("c_59"))
              )
            , Seq(
                Build(Var("b_59"))
              , Seq(
                  CallT(SVar("n_31"), [], [])
                , Seq(
                    Match(Var("d_59"))
                  , Build(
                      Anno(Op("String", [Var("d_59")]), Var("c_59"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Char_1_0"
        , [ VarDec(
              "o_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_59", "e_59", "g_59"]
          , Seq(
              Match(
                Anno(Op("Char", [Var("e_59")]), Var("f_59"))
              )
            , Seq(
                Build(Var("e_59"))
              , Seq(
                  CallT(SVar("o_31"), [], [])
                , Seq(
                    Match(Var("g_59"))
                  , Build(
                      Anno(Op("Char", [Var("g_59")]), Var("f_59"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QModId_2_0"
        , [ VarDec(
              "p_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_59", "h_59", "i_59", "k_59", "l_59"]
          , Seq(
              Match(
                Anno(
                  Op("QModId", [Var("h_59"), Var("i_59")])
                , Var("j_59")
                )
              )
            , Seq(
                Build(Var("h_59"))
              , Seq(
                  CallT(SVar("p_31"), [], [])
                , Seq(
                    Match(Var("k_59"))
                  , Seq(
                      Build(Var("i_59"))
                    , Seq(
                        CallT(SVar("q_31"), [], [])
                      , Seq(
                          Match(Var("l_59"))
                        , Build(
                            Anno(
                              Op("QModId", [Var("k_59"), Var("l_59")])
                            , Var("j_59")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QConSym_2_0"
        , [ VarDec(
              "r_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "s_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_59", "m_59", "n_59", "p_59", "q_59"]
          , Seq(
              Match(
                Anno(
                  Op("QConSym", [Var("m_59"), Var("n_59")])
                , Var("o_59")
                )
              )
            , Seq(
                Build(Var("m_59"))
              , Seq(
                  CallT(SVar("r_31"), [], [])
                , Seq(
                    Match(Var("p_59"))
                  , Seq(
                      Build(Var("n_59"))
                    , Seq(
                        CallT(SVar("s_31"), [], [])
                      , Seq(
                          Match(Var("q_59"))
                        , Build(
                            Anno(
                              Op("QConSym", [Var("p_59"), Var("q_59")])
                            , Var("o_59")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QVarSym_2_0"
        , [ VarDec(
              "t_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_59", "r_59", "s_59", "u_59", "v_59"]
          , Seq(
              Match(
                Anno(
                  Op("QVarSym", [Var("r_59"), Var("s_59")])
                , Var("t_59")
                )
              )
            , Seq(
                Build(Var("r_59"))
              , Seq(
                  CallT(SVar("t_31"), [], [])
                , Seq(
                    Match(Var("u_59"))
                  , Seq(
                      Build(Var("s_59"))
                    , Seq(
                        CallT(SVar("u_31"), [], [])
                      , Seq(
                          Match(Var("v_59"))
                        , Build(
                            Anno(
                              Op("QVarSym", [Var("u_59"), Var("v_59")])
                            , Var("t_59")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QConId_2_0"
        , [ VarDec(
              "v_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_59", "w_59", "x_59", "z_59", "a_60"]
          , Seq(
              Match(
                Anno(
                  Op("QConId", [Var("w_59"), Var("x_59")])
                , Var("y_59")
                )
              )
            , Seq(
                Build(Var("w_59"))
              , Seq(
                  CallT(SVar("v_31"), [], [])
                , Seq(
                    Match(Var("z_59"))
                  , Seq(
                      Build(Var("x_59"))
                    , Seq(
                        CallT(SVar("w_31"), [], [])
                      , Seq(
                          Match(Var("a_60"))
                        , Build(
                            Anno(
                              Op("QConId", [Var("z_59"), Var("a_60")])
                            , Var("y_59")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QVarId_2_0"
        , [ VarDec(
              "x_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_60", "b_60", "c_60", "e_60", "f_60"]
          , Seq(
              Match(
                Anno(
                  Op("QVarId", [Var("b_60"), Var("c_60")])
                , Var("d_60")
                )
              )
            , Seq(
                Build(Var("b_60"))
              , Seq(
                  CallT(SVar("x_31"), [], [])
                , Seq(
                    Match(Var("e_60"))
                  , Seq(
                      Build(Var("c_60"))
                    , Seq(
                        CallT(SVar("y_31"), [], [])
                      , Seq(
                          Match(Var("f_60"))
                        , Build(
                            Anno(
                              Op("QVarId", [Var("e_60"), Var("f_60")])
                            , Var("d_60")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "BinCon_1_0"
        , [ VarDec(
              "z_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_60", "g_60", "i_60"]
          , Seq(
              Match(
                Anno(Op("BinCon", [Var("g_60")]), Var("h_60"))
              )
            , Seq(
                Build(Var("g_60"))
              , Seq(
                  CallT(SVar("z_31"), [], [])
                , Seq(
                    Match(Var("i_60"))
                  , Build(
                      Anno(Op("BinCon", [Var("i_60")]), Var("h_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ConsOp_1_0"
        , [ VarDec(
              "a_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_60", "j_60", "l_60"]
          , Seq(
              Match(
                Anno(Op("ConsOp", [Var("j_60")]), Var("k_60"))
              )
            , Seq(
                Build(Var("j_60"))
              , Seq(
                  CallT(SVar("a_32"), [], [])
                , Seq(
                    Match(Var("l_60"))
                  , Build(
                      Anno(Op("ConsOp", [Var("l_60")]), Var("k_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QPrefCon_1_0"
        , [ VarDec(
              "b_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_60", "m_60", "o_60"]
          , Seq(
              Match(
                Anno(Op("QPrefCon", [Var("m_60")]), Var("n_60"))
              )
            , Seq(
                Build(Var("m_60"))
              , Seq(
                  CallT(SVar("b_32"), [], [])
                , Seq(
                    Match(Var("o_60"))
                  , Build(
                      Anno(Op("QPrefCon", [Var("o_60")]), Var("n_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrefCon_1_0"
        , [ VarDec(
              "c_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_60", "p_60", "r_60"]
          , Seq(
              Match(
                Anno(Op("PrefCon", [Var("p_60")]), Var("q_60"))
              )
            , Seq(
                Build(Var("p_60"))
              , Seq(
                  CallT(SVar("c_32"), [], [])
                , Seq(
                    Match(Var("r_60"))
                  , Build(
                      Anno(Op("PrefCon", [Var("r_60")]), Var("q_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "QPrefOp_1_0"
        , [ VarDec(
              "d_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_60", "s_60", "u_60"]
          , Seq(
              Match(
                Anno(Op("QPrefOp", [Var("s_60")]), Var("t_60"))
              )
            , Seq(
                Build(Var("s_60"))
              , Seq(
                  CallT(SVar("d_32"), [], [])
                , Seq(
                    Match(Var("u_60"))
                  , Build(
                      Anno(Op("QPrefOp", [Var("u_60")]), Var("t_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "PrefOp_1_0"
        , [ VarDec(
              "e_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_60", "v_60", "x_60"]
          , Seq(
              Match(
                Anno(Op("PrefOp", [Var("v_60")]), Var("w_60"))
              )
            , Seq(
                Build(Var("v_60"))
              , Seq(
                  CallT(SVar("e_32"), [], [])
                , Seq(
                    Match(Var("x_60"))
                  , Build(
                      Anno(Op("PrefOp", [Var("x_60")]), Var("w_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "ConOp_1_0"
        , [ VarDec(
              "f_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_60", "y_60", "a_61"]
          , Seq(
              Match(
                Anno(Op("ConOp", [Var("y_60")]), Var("z_60"))
              )
            , Seq(
                Build(Var("y_60"))
              , Seq(
                  CallT(SVar("f_32"), [], [])
                , Seq(
                    Match(Var("a_61"))
                  , Build(
                      Anno(Op("ConOp", [Var("a_61")]), Var("z_60"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Op_1_0"
        , [ VarDec(
              "g_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_61", "b_61", "d_61"]
          , Seq(
              Match(
                Anno(Op("Op", [Var("b_61")]), Var("c_61"))
              )
            , Seq(
                Build(Var("b_61"))
              , Seq(
                  CallT(SVar("g_32"), [], [])
                , Seq(
                    Match(Var("d_61"))
                  , Build(
                      Anno(Op("Op", [Var("d_61")]), Var("c_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "BinOpQ_1_0"
        , [ VarDec(
              "h_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_61", "e_61", "g_61"]
          , Seq(
              Match(
                Anno(Op("BinOpQ", [Var("e_61")]), Var("f_61"))
              )
            , Seq(
                Build(Var("e_61"))
              , Seq(
                  CallT(SVar("h_32"), [], [])
                , Seq(
                    Match(Var("g_61"))
                  , Build(
                      Anno(Op("BinOpQ", [Var("g_61")]), Var("f_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "BinOp_1_0"
        , [ VarDec(
              "i_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_61", "h_61", "j_61"]
          , Seq(
              Match(
                Anno(Op("BinOp", [Var("h_61")]), Var("i_61"))
              )
            , Seq(
                Build(Var("h_61"))
              , Seq(
                  CallT(SVar("i_32"), [], [])
                , Seq(
                    Match(Var("j_61"))
                  , Build(
                      Anno(Op("BinOp", [Var("j_61")]), Var("i_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Var_1_0"
        , [ VarDec(
              "j_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_61", "k_61", "m_61"]
          , Seq(
              Match(
                Anno(Op("Var", [Var("k_61")]), Var("l_61"))
              )
            , Seq(
                Build(Var("k_61"))
              , Seq(
                  CallT(SVar("j_32"), [], [])
                , Seq(
                    Match(Var("m_61"))
                  , Build(
                      Anno(Op("Var", [Var("m_61")]), Var("l_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "EmptyList_0_0"
        , []
        , []
        , Match(Anno(Op("EmptyList", []), Wld()))
        )
      , SDefT(
          "Unit_0_0"
        , []
        , []
        , Match(Anno(Op("Unit", []), Wld()))
        )
      , SDefT(
          "Ins_1_0"
        , [ VarDec(
              "k_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_61", "n_61", "p_61"]
          , Seq(
              Match(
                Anno(Op("Ins", [Var("n_61")]), Var("o_61"))
              )
            , Seq(
                Build(Var("n_61"))
              , Seq(
                  CallT(SVar("k_32"), [], [])
                , Seq(
                    Match(Var("p_61"))
                  , Build(
                      Anno(Op("Ins", [Var("p_61")]), Var("o_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Snoc_2_0"
        , [ VarDec(
              "l_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_61", "q_61", "r_61", "t_61", "u_61"]
          , Seq(
              Match(
                Anno(
                  Op("Snoc", [Var("q_61"), Var("r_61")])
                , Var("s_61")
                )
              )
            , Seq(
                Build(Var("q_61"))
              , Seq(
                  CallT(SVar("l_32"), [], [])
                , Seq(
                    Match(Var("t_61"))
                  , Seq(
                      Build(Var("r_61"))
                    , Seq(
                        CallT(SVar("m_32"), [], [])
                      , Seq(
                          Match(Var("u_61"))
                        , Build(
                            Anno(
                              Op("Snoc", [Var("t_61"), Var("u_61")])
                            , Var("s_61")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DR__UNDEFINE_1_0"
        , [ VarDec(
              "n_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_61", "v_61", "x_61"]
          , Seq(
              Match(
                Anno(Op("DR_UNDEFINE", [Var("v_61")]), Var("w_61"))
              )
            , Seq(
                Build(Var("v_61"))
              , Seq(
                  CallT(SVar("n_32"), [], [])
                , Seq(
                    Match(Var("x_61"))
                  , Build(
                      Anno(Op("DR_UNDEFINE", [Var("x_61")]), Var("w_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DR__DUMMY_0_0"
        , []
        , []
        , Match(Anno(Op("DR_DUMMY", []), Wld()))
        )
      ]
    )
  ]
)

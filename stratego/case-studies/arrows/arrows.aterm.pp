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
              "Var"
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
            , FunType([ConstType(SortNoArgs("Qop"))], ConstType(SortNoArgs("Qcon")))
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
              "VarVar"
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
            ["k_12", "l_12", "m_12", "n_12", "o_12", "q_12", "p_12", "r_12"]
          , Seq(
              Match(
                Anno(
                  Op("ArrProcedure", [Var("l_12"), Var("k_12")])
                , Wld()
                )
              )
            , Seq(
                Match(Var("n_12"))
              , Seq(
                  Build(Var("l_12"))
                , Seq(
                    CallT(SVar("free_pat_vars_0_0"), [], [])
                  , Seq(
                      Match(Var("m_12"))
                    , Seq(
                        Build(Var("n_12"))
                      , Seq(
                          Match(Var("q_12"))
                        , Seq(
                            Build(Var("m_12"))
                          , Seq(
                              CallT(SVar("tuple_0_0"), [], [])
                            , Seq(
                                Match(Var("o_12"))
                              , Seq(
                                  Build(Var("q_12"))
                                , Seq(
                                    Match(Var("r_12"))
                                  , Seq(
                                      Build(Var("k_12"))
                                    , Seq(
                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("m_12")])
                                      , Seq(
                                          Match(Var("p_12"))
                                        , Seq(
                                            Build(Var("r_12"))
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
                                                                  , [Var("l_12"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                  )
                                                                , Op("Nil", [])
                                                                )
                                                              , Var("o_12")
                                                              ]
                                                            )
                                                          , Op("Nil", [])
                                                          )
                                                        ]
                                                      )
                                                    , Op("Nil", [])
                                                    )
                                                  , Anno(Str(">>>"), Op("Nil", []))
                                                  , Var("p_12")
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
        , [VarDec("l_32", ConstType(Sort("ATerm", [])))]
        , GuardedLChoice(
            Scope(
              ["e_17", "f_17", "g_17", "h_17"]
            , Seq(
                Match(
                  Anno(
                    Op("ArrFirst", [Var("f_17"), Var("e_17")])
                  , Wld()
                  )
                )
              , Seq(
                  Match(Var("h_17"))
                , Seq(
                    Build(Var("l_32"))
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
                                            , Var("e_17")
                                            ]
                                          )
                                        , Op("Nil", [])
                                        )
                                      ]
                                    )
                                  , Op("Nil", [])
                                  )
                                , Anno(Str(">>>"), Op("Nil", []))
                                , Var("f_17")
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
                ["z_16", "a_17", "b_17", "c_17"]
              , Seq(
                  Match(
                    Anno(
                      Op("ArrHigher", [Var("z_16"), Var("a_17")])
                    , Wld()
                    )
                  )
                , Seq(
                    Match(Var("c_17"))
                  , Seq(
                      Build(Var("l_32"))
                    , Seq(
                        CallT(SVar("tuple_pat_0_0"), [], [])
                      , Seq(
                          Match(Var("b_17"))
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
                                                  , [Var("b_17"), Anno(Op("Nil", []), Op("Nil", []))]
                                                  )
                                                , Op("Nil", [])
                                                )
                                              , Anno(
                                                  Op(
                                                    "Product"
                                                  , [ Anno(
                                                        Op(
                                                          "ECons"
                                                        , [ Var("z_16")
                                                          , Anno(
                                                              Op(
                                                                "Cons"
                                                              , [Var("a_17"), Anno(Op("Nil", []), Op("Nil", []))]
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
                  [ "l_16"
                  , "m_16"
                  , "n_16"
                  , "o_16"
                  , "t_16"
                  , "p_16"
                  , "u_16"
                  , "q_16"
                  , "v_16"
                  , "r_16"
                  , "w_16"
                  , "s_16"
                  , "x_16"
                  ]
                , Seq(
                    Match(
                      Anno(
                        Op(
                          "ArrIf"
                        , [Var("l_16"), Var("m_16"), Var("n_16")]
                        )
                      , Wld()
                      )
                    )
                  , Seq(
                      Match(Var("t_16"))
                    , Seq(
                        Build(Var("l_32"))
                      , Seq(
                          CallT(SVar("tuple_pat_0_0"), [], [])
                        , Seq(
                            Match(Var("o_16"))
                          , Seq(
                              Build(Var("t_16"))
                            , Seq(
                                Match(Var("u_16"))
                              , Seq(
                                  Build(Var("l_32"))
                                , Seq(
                                    CallT(SVar("tuple_0_0"), [], [])
                                  , Seq(
                                      Match(Var("p_16"))
                                    , Seq(
                                        Build(Var("u_16"))
                                      , Seq(
                                          Match(Var("v_16"))
                                        , Seq(
                                            Build(Var("l_32"))
                                          , Seq(
                                              CallT(SVar("tuple_0_0"), [], [])
                                            , Seq(
                                                Match(Var("q_16"))
                                              , Seq(
                                                  Build(Var("v_16"))
                                                , Seq(
                                                    Match(Var("w_16"))
                                                  , Seq(
                                                      Build(Var("m_16"))
                                                    , Seq(
                                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                                      , Seq(
                                                          Match(Var("r_16"))
                                                        , Seq(
                                                            Build(Var("w_16"))
                                                          , Seq(
                                                              Match(Var("x_16"))
                                                            , Seq(
                                                                Build(Var("n_16"))
                                                              , Seq(
                                                                  CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                                                , Seq(
                                                                    Match(Var("s_16"))
                                                                  , Seq(
                                                                      Build(Var("x_16"))
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
                                                                                            , [Var("o_16"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                            )
                                                                                          , Op("Nil", [])
                                                                                          )
                                                                                        , Anno(
                                                                                            Op(
                                                                                              "If"
                                                                                            , [ Var("l_16")
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
                                                                                                    , Var("p_16")
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
                                                                                                    , Var("q_16")
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
                                                                                , [ Var("r_16")
                                                                                  , Anno(Str("|||"), Op("Nil", []))
                                                                                  , Var("s_16")
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
                    [ "z_15"
                    , "a_16"
                    , "b_16"
                    , "c_16"
                    , "d_16"
                    , "e_16"
                    , "h_16"
                    , "f_16"
                    , "i_16"
                    , "g_16"
                    , "j_16"
                    ]
                  , Seq(
                      Match(
                        Anno(
                          Op("ArrLet", [Var("a_16"), Var("z_15")])
                        , Wld()
                        )
                      )
                    , Seq(
                        Match(Var("d_16"))
                      , Seq(
                          Build(Var("a_16"))
                        , Seq(
                            CallT(SVar("free_decls_vars_0_0"), [], [])
                          , Seq(
                              Match(Var("b_16"))
                            , Seq(
                                Build(
                                  Anno(
                                    Op("", [Var("l_32"), Var("b_16")])
                                  , Op("Nil", [])
                                  )
                                )
                              , Seq(
                                  CallT(SVar("conc_0_0"), [], [])
                                , Seq(
                                    Match(Var("c_16"))
                                  , Seq(
                                      Build(Var("d_16"))
                                    , Seq(
                                        Match(Var("h_16"))
                                      , Seq(
                                          Build(Var("l_32"))
                                        , Seq(
                                            CallT(SVar("tuple_pat_0_0"), [], [])
                                          , Seq(
                                              Match(Var("e_16"))
                                            , Seq(
                                                Build(Var("h_16"))
                                              , Seq(
                                                  Match(Var("i_16"))
                                                , Seq(
                                                    Build(Var("c_16"))
                                                  , Seq(
                                                      CallT(SVar("tuple_0_0"), [], [])
                                                    , Seq(
                                                        Match(Var("f_16"))
                                                      , Seq(
                                                          Build(Var("i_16"))
                                                        , Seq(
                                                            Match(Var("j_16"))
                                                          , Seq(
                                                              Build(Var("z_15"))
                                                            , Seq(
                                                                CallT(SVar("desugar_arrow_p__0_1"), [], [Var("c_16")])
                                                              , Seq(
                                                                  Match(Var("g_16"))
                                                                , Seq(
                                                                    Build(Var("j_16"))
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
                                                                                          , [Var("e_16"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                          )
                                                                                        , Op("Nil", [])
                                                                                        )
                                                                                      , Anno(
                                                                                          Op("Let", [Var("a_16"), Var("f_16")])
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
                                                                          , Var("g_16")
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
                      [ "n_15"
                      , "o_15"
                      , "p_15"
                      , "q_15"
                      , "r_15"
                      , "s_15"
                      , "v_15"
                      , "t_15"
                      , "w_15"
                      , "u_15"
                      , "x_15"
                      ]
                    , Seq(
                        Match(
                          Anno(
                            Op(
                              "ArrAbs"
                            , [ Anno(
                                  Op(
                                    "Cons"
                                  , [Var("o_15"), Anno(Op("Nil", []), Wld())]
                                  )
                                , Wld()
                                )
                              , Var("n_15")
                              ]
                            )
                          , Wld()
                          )
                        )
                      , Seq(
                          Match(Var("r_15"))
                        , Seq(
                            Build(Var("o_15"))
                          , Seq(
                              CallT(SVar("free_pat_vars_0_0"), [], [])
                            , Seq(
                                Match(Var("p_15"))
                              , Seq(
                                  Build(
                                    Anno(
                                      Op("", [Var("l_32"), Var("p_15")])
                                    , Op("Nil", [])
                                    )
                                  )
                                , Seq(
                                    CallT(SVar("conc_0_0"), [], [])
                                  , Seq(
                                      Match(Var("q_15"))
                                    , Seq(
                                        Build(Var("r_15"))
                                      , Seq(
                                          Match(Var("v_15"))
                                        , Seq(
                                            Build(Var("l_32"))
                                          , Seq(
                                              CallT(SVar("tuple_pat_0_0"), [], [])
                                            , Seq(
                                                Match(Var("s_15"))
                                              , Seq(
                                                  Build(Var("v_15"))
                                                , Seq(
                                                    Match(Var("w_15"))
                                                  , Seq(
                                                      Build(Var("q_15"))
                                                    , Seq(
                                                        CallT(SVar("tuple_0_0"), [], [])
                                                      , Seq(
                                                          Match(Var("t_15"))
                                                        , Seq(
                                                            Build(Var("w_15"))
                                                          , Seq(
                                                              Match(Var("x_15"))
                                                            , Seq(
                                                                Build(Var("n_15"))
                                                              , Seq(
                                                                  CallT(SVar("desugar_arrow_p__0_1"), [], [Var("q_15")])
                                                                , Seq(
                                                                    Match(Var("u_15"))
                                                                  , Seq(
                                                                      Build(Var("x_15"))
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
                                                                                                  , [ Var("s_15")
                                                                                                    , Anno(
                                                                                                        Op(
                                                                                                          "Cons"
                                                                                                        , [Var("o_15"), Anno(Op("Nil", []), Op("Nil", []))]
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
                                                                                        , Var("t_15")
                                                                                        ]
                                                                                      )
                                                                                    , Op("Nil", [])
                                                                                    )
                                                                                  ]
                                                                                )
                                                                              , Op("Nil", [])
                                                                              )
                                                                            , Anno(Str(">>>"), Op("Nil", []))
                                                                            , Var("u_15")
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
                        ["e_15", "f_15", "g_15", "j_15", "h_15", "k_15", "i_15", "l_15"]
                      , Seq(
                          Match(
                            Anno(
                              Op("ArrAppBin", [Var("f_15"), Var("e_15")])
                            , Wld()
                            )
                          )
                        , Seq(
                            Match(Var("j_15"))
                          , Seq(
                              Build(Var("l_32"))
                            , Seq(
                                CallT(SVar("tuple_pat_0_0"), [], [])
                              , Seq(
                                  Match(Var("g_15"))
                                , Seq(
                                    Build(Var("j_15"))
                                  , Seq(
                                      Match(Var("k_15"))
                                    , Seq(
                                        Build(Var("l_32"))
                                      , Seq(
                                          CallT(SVar("tuple_0_0"), [], [])
                                        , Seq(
                                            Match(Var("h_15"))
                                          , Seq(
                                              Build(Var("k_15"))
                                            , Seq(
                                                Match(Var("l_15"))
                                              , Seq(
                                                  Build(Var("f_15"))
                                                , Seq(
                                                    CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                                  , Seq(
                                                      Match(Var("i_15"))
                                                    , Seq(
                                                        Build(Var("l_15"))
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
                                                                              , [Var("g_15"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                              )
                                                                            , Op("Nil", [])
                                                                            )
                                                                          , Anno(
                                                                              Op(
                                                                                "Product"
                                                                              , [ Anno(
                                                                                    Op(
                                                                                      "ECons"
                                                                                    , [ Var("h_15")
                                                                                      , Anno(
                                                                                          Op(
                                                                                            "Cons"
                                                                                          , [Var("e_15"), Anno(Op("Nil", []), Op("Nil", []))]
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
                                                              , Var("i_15")
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
                          [ "t_14"
                          , "u_14"
                          , "v_14"
                          , "w_14"
                          , "x_14"
                          , "z_14"
                          , "y_14"
                          , "a_15"
                          , "b_15"
                          , "c_15"
                          ]
                        , Seq(
                            Match(
                              Anno(
                                Op("ArrForm", [Var("t_14"), Var("u_14")])
                              , Wld()
                              )
                            )
                          , Seq(
                              Match(Var("w_14"))
                            , Seq(
                                Match(Var("z_14"))
                              , Seq(
                                  Build(Var("l_32"))
                                , Seq(
                                    CallT(SVar("tuple_pat_0_0"), [], [])
                                  , Seq(
                                      Match(Var("x_14"))
                                    , Seq(
                                        Build(Var("z_14"))
                                      , Seq(
                                          Match(Var("a_15"))
                                        , Seq(
                                            Build(Var("l_32"))
                                          , Seq(
                                              CallT(SVar("tuple_0_0"), [], [])
                                            , Seq(
                                                Match(Var("y_14"))
                                              , Seq(
                                                  Build(Var("a_15"))
                                                , Seq(
                                                    Build(
                                                      Anno(
                                                        Op(
                                                          "Abs"
                                                        , [ Anno(
                                                              Op(
                                                                "Cons"
                                                              , [Var("x_14"), Anno(Op("Nil", []), Op("Nil", []))]
                                                              )
                                                            , Op("Nil", [])
                                                            )
                                                          , Var("y_14")
                                                          ]
                                                        )
                                                      , Op("Nil", [])
                                                      )
                                                    )
                                                  , Seq(
                                                      Match(Var("v_14"))
                                                    , Seq(
                                                        Build(Var("w_14"))
                                                      , Seq(
                                                          Match(Var("c_15"))
                                                        , Seq(
                                                            Build(Var("u_14"))
                                                          , Seq(
                                                              CallT(
                                                                SVar("map_1_0")
                                                              , [CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])]
                                                              , []
                                                              )
                                                            , Seq(
                                                                Match(Var("b_15"))
                                                              , Seq(
                                                                  Build(Var("c_15"))
                                                                , Seq(
                                                                    Build(
                                                                      Anno(
                                                                        Op("", [Var("t_14"), Var("b_15")])
                                                                      , Op("Nil", [])
                                                                      )
                                                                    )
                                                                  , CallT(SVar("apply_all_0_1"), [], [Var("v_14")])
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
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
                            ["p_14", "q_14", "r_14"]
                          , Seq(
                              Match(
                                Anno(
                                  Op(
                                    "ArrOpApp"
                                  , [Var("q_14"), Var("p_14"), Var("r_14")]
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
                                          Op("BinCon", [Var("p_14")])
                                        , Op("Nil", [])
                                        )
                                      , Anno(
                                          Op(
                                            "Cons"
                                          , [ Var("q_14")
                                            , Anno(
                                                Op(
                                                  "Cons"
                                                , [Var("r_14"), Anno(Op("Nil", []), Op("Nil", []))]
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
                              , CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                              )
                            )
                          )
                        , Id()
                        , GuardedLChoice(
                            Scope(
                              ["n_14"]
                            , Seq(
                                Match(
                                  Anno(
                                    Op(
                                      "ArrDo"
                                    , [ Anno(
                                          Op(
                                            "ArrStmtList"
                                          , [Anno(Op("ArrCmdStmt", [Var("n_14")]), Wld())]
                                          )
                                        , Wld()
                                        )
                                      ]
                                    )
                                  , Wld()
                                  )
                                )
                              , Seq(
                                  Build(Var("n_14"))
                                , CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                )
                              )
                            )
                          , Id()
                          , GuardedLChoice(
                              Scope(
                                [ "b_14"
                                , "c_14"
                                , "d_14"
                                , "e_14"
                                , "f_14"
                                , "g_14"
                                , "j_14"
                                , "h_14"
                                , "k_14"
                                , "i_14"
                                , "l_14"
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
                                                  , [Anno(Op("ArrLetStmt", [Var("c_14")]), Wld()), Var("b_14")]
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
                                    Match(Var("f_14"))
                                  , Seq(
                                      Build(Var("c_14"))
                                    , Seq(
                                        CallT(SVar("free_decls_vars_0_0"), [], [])
                                      , Seq(
                                          Match(Var("d_14"))
                                        , Seq(
                                            Build(
                                              Anno(
                                                Op("", [Var("l_32"), Var("d_14")])
                                              , Op("Nil", [])
                                              )
                                            )
                                          , Seq(
                                              CallT(SVar("conc_0_0"), [], [])
                                            , Seq(
                                                Match(Var("e_14"))
                                              , Seq(
                                                  Build(Var("f_14"))
                                                , Seq(
                                                    Match(Var("j_14"))
                                                  , Seq(
                                                      Build(Var("l_32"))
                                                    , Seq(
                                                        CallT(SVar("tuple_pat_0_0"), [], [])
                                                      , Seq(
                                                          Match(Var("g_14"))
                                                        , Seq(
                                                            Build(Var("j_14"))
                                                          , Seq(
                                                              Match(Var("k_14"))
                                                            , Seq(
                                                                Build(Var("e_14"))
                                                              , Seq(
                                                                  CallT(SVar("tuple_0_0"), [], [])
                                                                , Seq(
                                                                    Match(Var("h_14"))
                                                                  , Seq(
                                                                      Build(Var("k_14"))
                                                                    , Seq(
                                                                        Match(Var("l_14"))
                                                                      , Seq(
                                                                          Build(
                                                                            Anno(
                                                                              Op(
                                                                                "ArrDo"
                                                                              , [Anno(
                                                                                   Op("ArrStmtList", [Var("b_14")])
                                                                                 , Op("Nil", [])
                                                                                 )]
                                                                              )
                                                                            , Op("Nil", [])
                                                                            )
                                                                          )
                                                                        , Seq(
                                                                            CallT(SVar("desugar_arrow_p__0_1"), [], [Var("e_14")])
                                                                          , Seq(
                                                                              Match(Var("i_14"))
                                                                            , Seq(
                                                                                Build(Var("l_14"))
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
                                                                                                      , [Var("g_14"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                      )
                                                                                                    , Op("Nil", [])
                                                                                                    )
                                                                                                  , Anno(
                                                                                                      Op("Let", [Var("c_14"), Var("h_14")])
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
                                                                                      , Var("i_14")
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
                                  [ "o_13"
                                  , "p_13"
                                  , "q_13"
                                  , "v_13"
                                  , "r_13"
                                  , "w_13"
                                  , "s_13"
                                  , "x_13"
                                  , "t_13"
                                  , "y_13"
                                  , "u_13"
                                  , "z_13"
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
                                                    , [Anno(Op("ArrCmdStmt", [Var("o_13")]), Wld()), Var("p_13")]
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
                                      Match(Var("v_13"))
                                    , Seq(
                                        Build(Var("l_32"))
                                      , Seq(
                                          CallT(SVar("tuple_pat_0_0"), [], [])
                                        , Seq(
                                            Match(Var("q_13"))
                                          , Seq(
                                              Build(Var("v_13"))
                                            , Seq(
                                                Match(Var("w_13"))
                                              , Seq(
                                                  Build(Var("l_32"))
                                                , Seq(
                                                    CallT(SVar("tuple_0_0"), [], [])
                                                  , Seq(
                                                      Match(Var("r_13"))
                                                    , Seq(
                                                        Build(Var("w_13"))
                                                      , Seq(
                                                          Match(Var("x_13"))
                                                        , Seq(
                                                            Build(Var("l_32"))
                                                          , Seq(
                                                              CallT(SVar("tuple_0_0"), [], [])
                                                            , Seq(
                                                                Match(Var("s_13"))
                                                              , Seq(
                                                                  Build(Var("x_13"))
                                                                , Seq(
                                                                    Match(Var("y_13"))
                                                                  , Seq(
                                                                      Build(Var("o_13"))
                                                                    , Seq(
                                                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                                                      , Seq(
                                                                          Match(Var("t_13"))
                                                                        , Seq(
                                                                            Build(Var("y_13"))
                                                                          , Seq(
                                                                              Match(Var("z_13"))
                                                                            , Seq(
                                                                                Build(
                                                                                  Anno(
                                                                                    Op(
                                                                                      "ArrDo"
                                                                                    , [Anno(
                                                                                         Op("ArrStmtList", [Var("p_13")])
                                                                                       , Op("Nil", [])
                                                                                       )]
                                                                                    )
                                                                                  , Op("Nil", [])
                                                                                  )
                                                                                )
                                                                              , Seq(
                                                                                  CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                                                                , Seq(
                                                                                    Match(Var("u_13"))
                                                                                  , Seq(
                                                                                      Build(Var("z_13"))
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
                                                                                                            , [Var("q_13"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                            )
                                                                                                          , Op("Nil", [])
                                                                                                          )
                                                                                                        , Anno(
                                                                                                            Op(
                                                                                                              "Product"
                                                                                                            , [ Anno(
                                                                                                                  Op(
                                                                                                                    "ECons"
                                                                                                                  , [ Var("r_13")
                                                                                                                    , Anno(
                                                                                                                        Op(
                                                                                                                          "Cons"
                                                                                                                        , [Var("s_13"), Anno(Op("Nil", []), Op("Nil", []))]
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
                                                                                                        , Var("t_13")
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
                                                                                                        , Var("u_13")
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
                                  [ "t_12"
                                  , "u_12"
                                  , "v_12"
                                  , "w_12"
                                  , "x_12"
                                  , "y_12"
                                  , "z_12"
                                  , "g_13"
                                  , "a_13"
                                  , "h_13"
                                  , "b_13"
                                  , "i_13"
                                  , "c_13"
                                  , "j_13"
                                  , "d_13"
                                  , "k_13"
                                  , "e_13"
                                  , "l_13"
                                  , "f_13"
                                  , "m_13"
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
                                                          Op("ArrBindStmt", [Var("v_12"), Var("t_12")])
                                                        , Wld()
                                                        )
                                                      , Var("u_12")
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
                                      Match(Var("y_12"))
                                    , Seq(
                                        Build(Var("v_12"))
                                      , Seq(
                                          CallT(SVar("free_pat_vars_0_0"), [], [])
                                        , Seq(
                                            Match(Var("w_12"))
                                          , Seq(
                                              Build(
                                                Anno(
                                                  Op("", [Var("w_12"), Var("l_32")])
                                                , Op("Nil", [])
                                                )
                                              )
                                            , Seq(
                                                CallT(SVar("conc_0_0"), [], [])
                                              , Seq(
                                                  Match(Var("x_12"))
                                                , Seq(
                                                    Build(Var("y_12"))
                                                  , Seq(
                                                      Match(Var("g_13"))
                                                    , Seq(
                                                        Build(Var("l_32"))
                                                      , Seq(
                                                          CallT(SVar("tuple_pat_0_0"), [], [])
                                                        , Seq(
                                                            Match(Var("z_12"))
                                                          , Seq(
                                                              Build(Var("g_13"))
                                                            , Seq(
                                                                Match(Var("h_13"))
                                                              , Seq(
                                                                  Build(Var("l_32"))
                                                                , Seq(
                                                                    CallT(SVar("tuple_0_0"), [], [])
                                                                  , Seq(
                                                                      Match(Var("a_13"))
                                                                    , Seq(
                                                                        Build(Var("h_13"))
                                                                      , Seq(
                                                                          Match(Var("i_13"))
                                                                        , Seq(
                                                                            Build(Var("l_32"))
                                                                          , Seq(
                                                                              CallT(SVar("tuple_0_0"), [], [])
                                                                            , Seq(
                                                                                Match(Var("b_13"))
                                                                              , Seq(
                                                                                  Build(Var("i_13"))
                                                                                , Seq(
                                                                                    Match(Var("j_13"))
                                                                                  , Seq(
                                                                                      Build(Var("t_12"))
                                                                                    , Seq(
                                                                                        CallT(SVar("desugar_arrow_p__0_1"), [], [Var("l_32")])
                                                                                      , Seq(
                                                                                          Match(Var("c_13"))
                                                                                        , Seq(
                                                                                            Build(Var("j_13"))
                                                                                          , Seq(
                                                                                              Match(Var("k_13"))
                                                                                            , Seq(
                                                                                                Build(Var("l_32"))
                                                                                              , Seq(
                                                                                                  CallT(SVar("tuple_pat_0_0"), [], [])
                                                                                                , Seq(
                                                                                                    Match(Var("d_13"))
                                                                                                  , Seq(
                                                                                                      Build(Var("k_13"))
                                                                                                    , Seq(
                                                                                                        Match(Var("l_13"))
                                                                                                      , Seq(
                                                                                                          Build(Var("x_12"))
                                                                                                        , Seq(
                                                                                                            CallT(SVar("tuple_0_0"), [], [])
                                                                                                          , Seq(
                                                                                                              Match(Var("e_13"))
                                                                                                            , Seq(
                                                                                                                Build(Var("l_13"))
                                                                                                              , Seq(
                                                                                                                  Match(Var("m_13"))
                                                                                                                , Seq(
                                                                                                                    Build(
                                                                                                                      Anno(
                                                                                                                        Op(
                                                                                                                          "ArrDo"
                                                                                                                        , [Anno(
                                                                                                                             Op("ArrStmtList", [Var("u_12")])
                                                                                                                           , Op("Nil", [])
                                                                                                                           )]
                                                                                                                        )
                                                                                                                      , Op("Nil", [])
                                                                                                                      )
                                                                                                                    )
                                                                                                                  , Seq(
                                                                                                                      CallT(SVar("desugar_arrow_p__0_1"), [], [Var("x_12")])
                                                                                                                    , Seq(
                                                                                                                        Match(Var("f_13"))
                                                                                                                      , Seq(
                                                                                                                          Build(Var("m_13"))
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
                                                                                                                                                , [Var("z_12"), Anno(Op("Nil", []), Op("Nil", []))]
                                                                                                                                                )
                                                                                                                                              , Op("Nil", [])
                                                                                                                                              )
                                                                                                                                            , Anno(
                                                                                                                                                Op(
                                                                                                                                                  "Product"
                                                                                                                                                , [ Anno(
                                                                                                                                                      Op(
                                                                                                                                                        "ECons"
                                                                                                                                                      , [ Var("a_13")
                                                                                                                                                        , Anno(
                                                                                                                                                            Op(
                                                                                                                                                              "Cons"
                                                                                                                                                            , [Var("b_13"), Anno(Op("Nil", []), Op("Nil", []))]
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
                                                                                                                                            , Var("c_13")
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
                                                                                                                                                                  , [ Var("v_12")
                                                                                                                                                                    , Anno(
                                                                                                                                                                        Op(
                                                                                                                                                                          "Cons"
                                                                                                                                                                        , [Var("d_13"), Anno(Op("Nil", []), Op("Nil", []))]
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
                                                                                                                                                        , Var("e_13")
                                                                                                                                                        ]
                                                                                                                                                      )
                                                                                                                                                    , Op("Nil", [])
                                                                                                                                                    )
                                                                                                                                                  ]
                                                                                                                                                )
                                                                                                                                              , Op("Nil", [])
                                                                                                                                              )
                                                                                                                                            , Anno(Str(">>>"), Op("Nil", []))
                                                                                                                                            , Var("f_13")
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
                ["k_17"]
              , Seq(
                  Match(
                    Anno(
                      Op(
                        "Cons"
                      , [Var("k_17"), Anno(Op("Nil", []), Wld())]
                      )
                    , Wld()
                    )
                  )
                , Build(Var("k_17"))
                )
              )
            , Id()
            , Scope(
                ["i_17", "j_17"]
              , Seq(
                  Match(
                    Anno(
                      Op("Cons", [Var("i_17"), Var("j_17")])
                    , Wld()
                    )
                  )
                , Build(
                    Anno(
                      Op("TuplePat", [Var("i_17"), Var("j_17")])
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
        , Seq(
            CallT(
              SVar("map_1_0")
            , [ Scope(
                  ["l_17"]
                , Seq(
                    Match(Anno(Op("VarVar", [Var("l_17")]), Wld()))
                  , Build(
                      Anno(
                        Op("Var", [Var("l_17")])
                      , Op("Nil", [])
                      )
                    )
                  )
                )
              ]
            , []
            )
          , CallT(SVar("tuple_exp_0_0"), [], [])
          )
        )
      , SDefT(
          "tuple_exp_0_0"
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
                ["o_17"]
              , Seq(
                  Match(
                    Anno(
                      Op(
                        "Cons"
                      , [Var("o_17"), Anno(Op("Nil", []), Wld())]
                      )
                    , Wld()
                    )
                  )
                , Build(Var("o_17"))
                )
              )
            , Id()
            , Scope(
                ["m_17", "n_17"]
              , Seq(
                  Match(
                    Anno(
                      Op("Cons", [Var("m_17"), Var("n_17")])
                    , Wld()
                    )
                  )
                , Build(
                    Anno(
                      Op(
                        "Product"
                      , [ Anno(
                            Op("ECons", [Var("m_17"), Var("n_17")])
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
          , [Match(Anno(Op("VarVar", [Wld()]), Wld()))]
          , []
          )
        )
      , SDefT(
          "free_decls_vars_0_0"
        , []
        , []
        , CallT(
            SVar("collect_all_3_0")
          , [ Match(Anno(Op("VarVar", [Wld()]), Wld()))
            , CallT(SVar("union_0_0"), [], [])
            , Scope(
                ["p_17"]
              , Seq(
                  Match(
                    Anno(Op("VarFunLHS", [Var("p_17"), Wld()]), Wld())
                  )
                , Build(Var("p_17"))
                )
              )
            ]
          , []
          )
        )
      , SDefT(
          "apply_all_0_1"
        , []
        , [VarDec("m_32", ConstType(Sort("ATerm", [])))]
        , GuardedLChoice(
            Scope(
              ["v_17"]
            , Seq(
                Match(
                  Anno(
                    Op(
                      ""
                    , [Var("v_17"), Anno(Op("Nil", []), Wld())]
                    )
                  , Wld()
                  )
                )
              , Build(Var("v_17"))
              )
            )
          , Id()
          , Scope(
              ["r_17", "s_17", "t_17"]
            , Seq(
                Match(
                  Anno(
                    Op(
                      ""
                    , [ Var("r_17")
                      , Anno(
                          Op("Cons", [Var("s_17"), Var("t_17")])
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
                            , [ Var("r_17")
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
                                          , Var("m_32")
                                          ]
                                        )
                                      , Op("Nil", [])
                                      )
                                    , Anno(Str(">>>"), Op("Nil", []))
                                    , Var("s_17")
                                    ]
                                  )
                                , Op("Nil", [])
                                )
                              ]
                            )
                          , Op("Nil", [])
                          )
                        , Var("t_17")
                        ]
                      )
                    , Op("Nil", [])
                    )
                  )
                , CallT(SVar("apply_all_0_1"), [], [Var("m_32")])
                )
              )
            )
          )
        )
      , SDefT(
          "map_1_0"
        , [ VarDec(
              "n_32"
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
            , Build(Anno(Op("Nil", []), Op("Nil", [])))
            )
          , Id()
          , Scope(
              ["w_17", "x_17", "z_17", "b_18", "a_18", "c_18"]
            , Seq(
                Match(
                  Anno(
                    Op("Cons", [Var("w_17"), Var("x_17")])
                  , Wld()
                  )
                )
              , Seq(
                  Match(Var("b_18"))
                , Seq(
                    Build(Var("w_17"))
                  , Seq(
                      CallT(SVar("n_32"), [], [])
                    , Seq(
                        Match(Var("z_17"))
                      , Seq(
                          Build(Var("b_18"))
                        , Seq(
                            Match(Var("c_18"))
                          , Seq(
                              Build(Var("x_17"))
                            , Seq(
                                CallT(
                                  SVar("map_1_0")
                                , [CallT(SVar("n_32"), [], [])]
                                , []
                                )
                              , Seq(
                                  Match(Var("a_18"))
                                , Seq(
                                    Build(Var("c_18"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("z_17"), Var("a_18")])
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
      , SDefT(
          "collect_all_1_0"
        , [ VarDec(
              "e_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , CallT(
            SVar("collect_all_2_0")
          , [ CallT(SVar("e_18"), [], [])
            , CallT(SVar("union_0_0"), [], [])
            ]
          , []
          )
        )
      , SDefT(
          "collect_all_2_0"
        , [ VarDec(
              "f_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "h_18"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["i_18", "k_18", "j_18", "l_18"]
                  , Seq(
                      Match(Var("k_18"))
                    , Seq(
                        CallT(SVar("f_18"), [], [])
                      , Seq(
                          Match(Var("i_18"))
                        , Seq(
                            Build(Var("k_18"))
                          , Seq(
                              Match(Var("l_18"))
                            , Seq(
                                CallT(
                                  SVar("crush_3_0")
                                , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                                  , CallT(SVar("g_18"), [], [])
                                  , CallT(SVar("h_18"), [], [])
                                  ]
                                , []
                                )
                              , Seq(
                                  Match(Var("j_18"))
                                , Seq(
                                    Build(Var("l_18"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("i_18"), Var("j_18")])
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
                    , CallT(SVar("g_18"), [], [])
                    , CallT(SVar("h_18"), [], [])
                    ]
                  , []
                  )
                )
              )
            ]
          , CallT(SVar("h_18"), [], [])
          )
        )
      , SDefT(
          "collect_all_3_0"
        , [ VarDec(
              "m_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "p_18"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["q_18", "s_18", "r_18", "t_18"]
                  , Seq(
                      Match(Var("s_18"))
                    , Seq(
                        CallT(SVar("m_18"), [], [])
                      , Seq(
                          Match(Var("q_18"))
                        , Seq(
                            Build(Var("s_18"))
                          , Seq(
                              Match(Var("t_18"))
                            , Seq(
                                CallT(
                                  SVar("crush_3_0")
                                , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                                  , CallT(SVar("n_18"), [], [])
                                  , CallT(SVar("p_18"), [], [])
                                  ]
                                , []
                                )
                              , Seq(
                                  Match(Var("r_18"))
                                , Seq(
                                    Build(Var("t_18"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("q_18"), Var("r_18")])
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
                      CallT(SVar("o_18"), [], [])
                    , CallT(SVar("p_18"), [], [])
                    )
                  , Id()
                  , CallT(
                      SVar("crush_3_0")
                    , [ Build(Anno(Op("Nil", []), Op("Nil", [])))
                      , CallT(SVar("n_18"), [], [])
                      , CallT(SVar("p_18"), [], [])
                      ]
                    , []
                    )
                  )
                )
              )
            ]
          , CallT(SVar("p_18"), [], [])
          )
        )
      , SDefT(
          "crush_3_0"
        , [ VarDec(
              "v_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_18"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_18"]
          , Seq(
              Match(Anno(Explode(Wld(), Var("u_18")), Wld()))
            , Seq(
                Build(Var("u_18"))
              , CallT(
                  SVar("foldr_3_0")
                , [ CallT(SVar("v_18"), [], [])
                  , CallT(SVar("w_18"), [], [])
                  , CallT(SVar("x_18"), [], [])
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
              "a_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "c_19"
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
            , CallT(SVar("a_19"), [], [])
            )
          , Id()
          , Scope(
              ["y_18", "z_18", "d_19", "f_19", "e_19", "g_19"]
            , Seq(
                Match(
                  Anno(
                    Op("Cons", [Var("y_18"), Var("z_18")])
                  , Wld()
                  )
                )
              , Seq(
                  Match(Var("f_19"))
                , Seq(
                    Build(Var("y_18"))
                  , Seq(
                      CallT(SVar("c_19"), [], [])
                    , Seq(
                        Match(Var("d_19"))
                      , Seq(
                          Build(Var("f_19"))
                        , Seq(
                            Match(Var("g_19"))
                          , Seq(
                              Build(Var("z_18"))
                            , Seq(
                                CallT(
                                  SVar("foldr_3_0")
                                , [ CallT(SVar("a_19"), [], [])
                                  , CallT(SVar("b_19"), [], [])
                                  , CallT(SVar("c_19"), [], [])
                                  ]
                                , []
                                )
                              , Seq(
                                  Match(Var("e_19"))
                                , Seq(
                                    Build(Var("g_19"))
                                  , Seq(
                                      Build(
                                        Anno(
                                          Op("", [Var("d_19"), Var("e_19")])
                                        , Op("Nil", [])
                                        )
                                      )
                                    , CallT(SVar("b_19"), [], [])
                                    )
                                  )
                                )
                              )
                            )
                          )
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
              ["h_19", "i_19"]
            , Seq(
                Match(
                  Anno(
                    Op("", [Var("h_19"), Var("i_19")])
                  , Wld()
                  )
                )
              , Seq(
                  Build(Var("h_19"))
                , CallT(SVar("at_end_1_0"), [Build(Var("i_19"))], [])
                )
              )
            )
          , Id()
          , Scope(
              ["j_19"]
            , Seq(
                Match(
                  Anno(
                    Explode(Anno(Str(""), Wld()), Var("j_19"))
                  , Wld()
                  )
                )
              , Seq(
                  Build(Var("j_19"))
                , CallT(SVar("concat_0_0"), [], [])
                )
              )
            )
          )
        )
      , SDefT(
          "at_end_1_0"
        , [ VarDec(
              "p_19"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "q_19"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["k_19", "l_19", "m_19", "n_19", "o_19"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("k_19"), Var("l_19")])
                        , Var("o_19")
                        )
                      )
                    , Seq(
                        Build(Var("k_19"))
                      , Seq(
                          Match(Var("m_19"))
                        , Seq(
                            Build(Var("l_19"))
                          , Seq(
                              CallT(SVar("q_19"), [], [])
                            , Seq(
                                Match(Var("n_19"))
                              , Build(
                                  Anno(
                                    Op("Cons", [Var("m_19"), Var("n_19")])
                                  , Var("o_19")
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
                  , CallT(SVar("p_19"), [], [])
                  )
                )
              )
            ]
          , CallT(SVar("q_19"), [], [])
          )
        )
      , SDefT(
          "concat_0_0"
        , []
        , []
        , Let(
            [ SDefT(
                "t_19"
              , []
              , []
              , GuardedLChoice(
                  Match(Anno(Op("Nil", []), Wld()))
                , Id()
                , Scope(
                    ["r_19", "s_19"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("r_19"), Var("s_19")])
                        , Wld()
                        )
                      )
                    , Seq(
                        Build(Var("r_19"))
                      , CallT(
                          SVar("at_end_1_0")
                        , [Seq(
                             Build(Var("s_19"))
                           , CallT(SVar("t_19"), [], [])
                           )]
                        , []
                        )
                      )
                    )
                  )
                )
              )
            ]
          , CallT(SVar("t_19"), [], [])
          )
        )
      , SDefT(
          "union_0_0"
        , []
        , []
        , Scope(
            ["u_19", "v_19"]
          , Let(
              [ SDefT(
                  "b_20"
                , []
                , []
                , GuardedLChoice(
                    Seq(
                      Match(Anno(Op("Nil", []), Wld()))
                    , Build(Var("u_19"))
                    )
                  , Id()
                  , GuardedLChoice(
                      Seq(
                        CallT(SVar("HdMember_1_0"), [Build(Var("u_19"))], [])
                      , CallT(SVar("b_20"), [], [])
                      )
                    , Id()
                    , Scope(
                        ["w_19", "x_19", "y_19", "z_19", "a_20"]
                      , Seq(
                          Match(
                            Anno(
                              Op("Cons", [Var("w_19"), Var("x_19")])
                            , Var("a_20")
                            )
                          )
                        , Seq(
                            Build(Var("w_19"))
                          , Seq(
                              Match(Var("y_19"))
                            , Seq(
                                Build(Var("x_19"))
                              , Seq(
                                  CallT(SVar("b_20"), [], [])
                                , Seq(
                                    Match(Var("z_19"))
                                  , Build(
                                      Anno(
                                        Op("Cons", [Var("y_19"), Var("z_19")])
                                      , Var("a_20")
                                      )
                                    )
                                  )
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
                    Op("", [Var("v_19"), Var("u_19")])
                  , Wld()
                  )
                )
              , Seq(
                  Build(Var("v_19"))
                , CallT(SVar("b_20"), [], [])
                )
              )
            )
          )
        )
      , SDefT(
          "HdMember_1_0"
        , [ VarDec(
              "f_20"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_20", "d_20", "g_20"]
          , Seq(
              Match(
                Anno(
                  Op("Cons", [Var("d_20"), Var("c_20")])
                , Wld()
                )
              )
            , Seq(
                Match(Var("g_20"))
              , Seq(
                  CallT(SVar("f_20"), [], [])
                , Seq(
                    CallT(
                      SVar("fetch_1_0")
                    , [ Scope(
                          ["e_20"]
                        , Seq(
                            Match(Var("e_20"))
                          , Seq(
                              Build(
                                Anno(
                                  Op("", [Var("d_20"), Var("e_20")])
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
                  , Seq(Build(Var("g_20")), Build(Var("c_20")))
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "fetch_1_0"
        , [ VarDec(
              "r_20"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Let(
            [ SDefT(
                "s_20"
              , []
              , []
              , GuardedLChoice(
                  Scope(
                    ["h_20", "i_20", "j_20", "k_20", "l_20"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("h_20"), Var("i_20")])
                        , Var("l_20")
                        )
                      )
                    , Seq(
                        Build(Var("h_20"))
                      , Seq(
                          CallT(SVar("r_20"), [], [])
                        , Seq(
                            Match(Var("j_20"))
                          , Seq(
                              Build(Var("i_20"))
                            , Seq(
                                Match(Var("k_20"))
                              , Build(
                                  Anno(
                                    Op("Cons", [Var("j_20"), Var("k_20")])
                                  , Var("l_20")
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
                    ["m_20", "n_20", "o_20", "p_20", "q_20"]
                  , Seq(
                      Match(
                        Anno(
                          Op("Cons", [Var("m_20"), Var("n_20")])
                        , Var("q_20")
                        )
                      )
                    , Seq(
                        Build(Var("m_20"))
                      , Seq(
                          Match(Var("o_20"))
                        , Seq(
                            Build(Var("n_20"))
                          , Seq(
                              CallT(SVar("s_20"), [], [])
                            , Seq(
                                Match(Var("p_20"))
                              , Build(
                                  Anno(
                                    Op("Cons", [Var("o_20"), Var("p_20")])
                                  , Var("q_20")
                                  )
                                )
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
          , CallT(SVar("s_20"), [], [])
          )
        )
      , SDefT(
          "eq_0_0"
        , []
        , []
        , Scope(
            ["t_20"]
          , Match(
              Anno(
                Op("", [Var("t_20"), Var("t_20")])
              , Wld()
              )
            )
          )
        )
      , SDefT(
          "oncetd_1_0"
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
                  CallT(SVar("u_20"), [], [])
                , Id()
                , One(CallT(SVar("v_20"), [], []))
                )
              )
            ]
          , CallT(SVar("v_20"), [], [])
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
              "a_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_20", "x_20", "y_20", "z_20"]
          , Seq(
              Match(Anno(Var("w_20"), Var("x_20")))
            , Seq(
                Build(Var("w_20"))
              , Seq(
                  CallT(SVar("a_21"), [], [])
                , Seq(
                    Match(Var("y_20"))
                  , Seq(
                      Build(Var("x_20"))
                    , Seq(
                        CallT(SVar("b_21"), [], [])
                      , Seq(
                          Match(Var("z_20"))
                        , Build(Anno(Var("y_20"), Var("z_20")))
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
              "c_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_32", "o_32", "p_32", "r_32", "s_32"]
          , Seq(
              Match(
                Anno(
                  Op("Cons", [Var("o_32"), Var("p_32")])
                , Var("q_32")
                )
              )
            , Seq(
                Build(Var("o_32"))
              , Seq(
                  CallT(SVar("c_21"), [], [])
                , Seq(
                    Match(Var("r_32"))
                  , Seq(
                      Build(Var("p_32"))
                    , Seq(
                        CallT(SVar("d_21"), [], [])
                      , Seq(
                          Match(Var("s_32"))
                        , Build(
                            Anno(
                              Op("Cons", [Var("r_32"), Var("s_32")])
                            , Var("q_32")
                            )
                          )
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
              "e_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_32", "t_32", "u_32", "w_32", "x_32"]
          , Seq(
              Match(
                Anno(
                  Op("Conc", [Var("t_32"), Var("u_32")])
                , Var("v_32")
                )
              )
            , Seq(
                Build(Var("t_32"))
              , Seq(
                  CallT(SVar("e_21"), [], [])
                , Seq(
                    Match(Var("w_32"))
                  , Seq(
                      Build(Var("u_32"))
                    , Seq(
                        CallT(SVar("f_21"), [], [])
                      , Seq(
                          Match(Var("x_32"))
                        , Build(
                            Anno(
                              Op("Conc", [Var("w_32"), Var("x_32")])
                            , Var("v_32")
                            )
                          )
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
              "g_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "h_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_33", "y_32", "z_32", "b_33", "c_33"]
          , Seq(
              Match(
                Anno(
                  Op("", [Var("y_32"), Var("z_32")])
                , Var("a_33")
                )
              )
            , Seq(
                Build(Var("y_32"))
              , Seq(
                  CallT(SVar("g_21"), [], [])
                , Seq(
                    Match(Var("b_33"))
                  , Seq(
                      Build(Var("z_32"))
                    , Seq(
                        CallT(SVar("h_21"), [], [])
                      , Seq(
                          Match(Var("c_33"))
                        , Build(
                            Anno(
                              Op("", [Var("b_33"), Var("c_33")])
                            , Var("a_33")
                            )
                          )
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
              "i_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["g_33", "d_33", "e_33", "f_33", "h_33", "i_33", "j_33"]
          , Seq(
              Match(
                Anno(
                  Op(
                    ""
                  , [Var("d_33"), Var("e_33"), Var("f_33")]
                  )
                , Var("g_33")
                )
              )
            , Seq(
                Build(Var("d_33"))
              , Seq(
                  CallT(SVar("i_21"), [], [])
                , Seq(
                    Match(Var("h_33"))
                  , Seq(
                      Build(Var("e_33"))
                    , Seq(
                        CallT(SVar("j_21"), [], [])
                      , Seq(
                          Match(Var("i_33"))
                        , Seq(
                            Build(Var("f_33"))
                          , Seq(
                              CallT(SVar("k_21"), [], [])
                            , Seq(
                                Match(Var("j_33"))
                              , Build(
                                  Anno(
                                    Op(
                                      ""
                                    , [Var("h_33"), Var("i_33"), Var("j_33")]
                                    )
                                  , Var("g_33")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["m_33", "k_33", "l_33", "n_33", "o_33"]
          , Seq(
              Match(
                Anno(
                  Op("StmtSeqOff", [Var("k_33"), Var("l_33")])
                , Var("m_33")
                )
              )
            , Seq(
                Build(Var("k_33"))
              , Seq(
                  CallT(SVar("l_21"), [], [])
                , Seq(
                    Match(Var("n_33"))
                  , Seq(
                      Build(Var("l_33"))
                    , Seq(
                        CallT(SVar("m_21"), [], [])
                      , Seq(
                          Match(Var("o_33"))
                        , Build(
                            Anno(
                              Op("StmtSeqOff", [Var("n_33"), Var("o_33")])
                            , Var("m_33")
                            )
                          )
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
              "n_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_33", "p_33", "q_33", "s_33", "t_33"]
          , Seq(
              Match(
                Anno(
                  Op("DeclSeqOff", [Var("p_33"), Var("q_33")])
                , Var("r_33")
                )
              )
            , Seq(
                Build(Var("p_33"))
              , Seq(
                  CallT(SVar("n_21"), [], [])
                , Seq(
                    Match(Var("s_33"))
                  , Seq(
                      Build(Var("q_33"))
                    , Seq(
                        CallT(SVar("o_21"), [], [])
                      , Seq(
                          Match(Var("t_33"))
                        , Build(
                            Anno(
                              Op("DeclSeqOff", [Var("s_33"), Var("t_33")])
                            , Var("r_33")
                            )
                          )
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
              "p_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_33", "u_33", "v_33", "x_33", "y_33"]
          , Seq(
              Match(
                Anno(
                  Op("AltSeqOff", [Var("u_33"), Var("v_33")])
                , Var("w_33")
                )
              )
            , Seq(
                Build(Var("u_33"))
              , Seq(
                  CallT(SVar("p_21"), [], [])
                , Seq(
                    Match(Var("x_33"))
                  , Seq(
                      Build(Var("v_33"))
                    , Seq(
                        CallT(SVar("q_21"), [], [])
                      , Seq(
                          Match(Var("y_33"))
                        , Build(
                            Anno(
                              Op("AltSeqOff", [Var("x_33"), Var("y_33")])
                            , Var("w_33")
                            )
                          )
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
              "r_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "s_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_34", "z_33", "a_34", "c_34", "d_34"]
          , Seq(
              Match(
                Anno(
                  Op("TopdeclSeqOff", [Var("z_33"), Var("a_34")])
                , Var("b_34")
                )
              )
            , Seq(
                Build(Var("z_33"))
              , Seq(
                  CallT(SVar("r_21"), [], [])
                , Seq(
                    Match(Var("c_34"))
                  , Seq(
                      Build(Var("a_34"))
                    , Seq(
                        CallT(SVar("s_21"), [], [])
                      , Seq(
                          Match(Var("d_34"))
                        , Build(
                            Anno(
                              Op("TopdeclSeqOff", [Var("c_34"), Var("d_34")])
                            , Var("b_34")
                            )
                          )
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
              "t_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_34", "e_34", "f_34", "h_34", "i_34"]
          , Seq(
              Match(
                Anno(
                  Op("ImportdeclSeqOff", [Var("e_34"), Var("f_34")])
                , Var("g_34")
                )
              )
            , Seq(
                Build(Var("e_34"))
              , Seq(
                  CallT(SVar("t_21"), [], [])
                , Seq(
                    Match(Var("h_34"))
                  , Seq(
                      Build(Var("f_34"))
                    , Seq(
                        CallT(SVar("u_21"), [], [])
                      , Seq(
                          Match(Var("i_34"))
                        , Build(
                            Anno(
                              Op("ImportdeclSeqOff", [Var("h_34"), Var("i_34")])
                            , Var("g_34")
                            )
                          )
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
              "v_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_34", "j_34", "l_34"]
          , Seq(
              Match(
                Anno(Op("FloatHash", [Var("j_34")]), Var("k_34"))
              )
            , Seq(
                Build(Var("j_34"))
              , Seq(
                  CallT(SVar("v_21"), [], [])
                , Seq(
                    Match(Var("l_34"))
                  , Build(
                      Anno(Op("FloatHash", [Var("l_34")]), Var("k_34"))
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
              "w_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_34", "m_34", "o_34"]
          , Seq(
              Match(
                Anno(Op("IntegerHash", [Var("m_34")]), Var("n_34"))
              )
            , Seq(
                Build(Var("m_34"))
              , Seq(
                  CallT(SVar("w_21"), [], [])
                , Seq(
                    Match(Var("o_34"))
                  , Build(
                      Anno(Op("IntegerHash", [Var("o_34")]), Var("n_34"))
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
              "x_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_34", "p_34", "r_34"]
          , Seq(
              Match(
                Anno(Op("StringHash", [Var("p_34")]), Var("q_34"))
              )
            , Seq(
                Build(Var("p_34"))
              , Seq(
                  CallT(SVar("x_21"), [], [])
                , Seq(
                    Match(Var("r_34"))
                  , Build(
                      Anno(Op("StringHash", [Var("r_34")]), Var("q_34"))
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
              "y_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_34", "s_34", "u_34"]
          , Seq(
              Match(
                Anno(Op("CharHash", [Var("s_34")]), Var("t_34"))
              )
            , Seq(
                Build(Var("s_34"))
              , Seq(
                  CallT(SVar("y_21"), [], [])
                , Seq(
                    Match(Var("u_34"))
                  , Build(
                      Anno(Op("CharHash", [Var("u_34")]), Var("t_34"))
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
              "z_21"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_34", "v_34", "x_34"]
          , Seq(
              Match(
                Anno(Op("FlexibleContext", [Var("v_34")]), Var("w_34"))
              )
            , Seq(
                Build(Var("v_34"))
              , Seq(
                  CallT(SVar("z_21"), [], [])
                , Seq(
                    Match(Var("x_34"))
                  , Build(
                      Anno(Op("FlexibleContext", [Var("x_34")]), Var("w_34"))
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
              "a_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_35", "y_34", "z_34", "b_35", "c_35"]
          , Seq(
              Match(
                Anno(
                  Op("SimpleClassFle", [Var("y_34"), Var("z_34")])
                , Var("a_35")
                )
              )
            , Seq(
                Build(Var("y_34"))
              , Seq(
                  CallT(SVar("a_22"), [], [])
                , Seq(
                    Match(Var("b_35"))
                  , Seq(
                      Build(Var("z_34"))
                    , Seq(
                        CallT(SVar("b_22"), [], [])
                      , Seq(
                          Match(Var("c_35"))
                        , Build(
                            Anno(
                              Op("SimpleClassFle", [Var("b_35"), Var("c_35")])
                            , Var("a_35")
                            )
                          )
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
              "c_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_35", "d_35", "e_35", "g_35", "h_35"]
          , Seq(
              Match(
                Anno(
                  Op("ClassFlex", [Var("d_35"), Var("e_35")])
                , Var("f_35")
                )
              )
            , Seq(
                Build(Var("d_35"))
              , Seq(
                  CallT(SVar("c_22"), [], [])
                , Seq(
                    Match(Var("g_35"))
                  , Seq(
                      Build(Var("e_35"))
                    , Seq(
                        CallT(SVar("d_22"), [], [])
                      , Seq(
                          Match(Var("h_35"))
                        , Build(
                            Anno(
                              Op("ClassFlex", [Var("g_35"), Var("h_35")])
                            , Var("f_35")
                            )
                          )
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
              "e_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_35", "i_35", "j_35", "l_35", "m_35"]
          , Seq(
              Match(
                Anno(
                  Op("StmtSeq", [Var("i_35"), Var("j_35")])
                , Var("k_35")
                )
              )
            , Seq(
                Build(Var("i_35"))
              , Seq(
                  CallT(SVar("e_22"), [], [])
                , Seq(
                    Match(Var("l_35"))
                  , Seq(
                      Build(Var("j_35"))
                    , Seq(
                        CallT(SVar("f_22"), [], [])
                      , Seq(
                          Match(Var("m_35"))
                        , Build(
                            Anno(
                              Op("StmtSeq", [Var("l_35"), Var("m_35")])
                            , Var("k_35")
                            )
                          )
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
              "g_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_35", "n_35", "p_35"]
          , Seq(
              Match(
                Anno(Op("StmtList", [Var("n_35")]), Var("o_35"))
              )
            , Seq(
                Build(Var("n_35"))
              , Seq(
                  CallT(SVar("g_22"), [], [])
                , Seq(
                    Match(Var("p_35"))
                  , Build(
                      Anno(Op("StmtList", [Var("p_35")]), Var("o_35"))
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
            ["s_35", "q_35", "r_35", "t_35", "u_35"]
          , Seq(
              Match(
                Anno(
                  Op("FBind", [Var("q_35"), Var("r_35")])
                , Var("s_35")
                )
              )
            , Seq(
                Build(Var("q_35"))
              , Seq(
                  CallT(SVar("h_22"), [], [])
                , Seq(
                    Match(Var("t_35"))
                  , Seq(
                      Build(Var("r_35"))
                    , Seq(
                        CallT(SVar("i_22"), [], [])
                      , Seq(
                          Match(Var("u_35"))
                        , Build(
                            Anno(
                              Op("FBind", [Var("t_35"), Var("u_35")])
                            , Var("s_35")
                            )
                          )
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
              "j_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_35", "v_35", "x_35"]
          , Seq(
              Match(
                Anno(Op("LetStmt", [Var("v_35")]), Var("w_35"))
              )
            , Seq(
                Build(Var("v_35"))
              , Seq(
                  CallT(SVar("j_22"), [], [])
                , Seq(
                    Match(Var("x_35"))
                  , Build(
                      Anno(Op("LetStmt", [Var("x_35")]), Var("w_35"))
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
              "k_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_35", "y_35", "a_36"]
          , Seq(
              Match(
                Anno(Op("ExpStmt", [Var("y_35")]), Var("z_35"))
              )
            , Seq(
                Build(Var("y_35"))
              , Seq(
                  CallT(SVar("k_22"), [], [])
                , Seq(
                    Match(Var("a_36"))
                  , Build(
                      Anno(Op("ExpStmt", [Var("a_36")]), Var("z_35"))
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
              "l_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_36", "b_36", "c_36", "e_36", "f_36"]
          , Seq(
              Match(
                Anno(
                  Op("BindStmt", [Var("b_36"), Var("c_36")])
                , Var("d_36")
                )
              )
            , Seq(
                Build(Var("b_36"))
              , Seq(
                  CallT(SVar("l_22"), [], [])
                , Seq(
                    Match(Var("e_36"))
                  , Seq(
                      Build(Var("c_36"))
                    , Seq(
                        CallT(SVar("m_22"), [], [])
                      , Seq(
                          Match(Var("f_36"))
                        , Build(
                            Anno(
                              Op("BindStmt", [Var("e_36"), Var("f_36")])
                            , Var("d_36")
                            )
                          )
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
              "n_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_36", "g_36", "h_36", "j_36", "k_36"]
          , Seq(
              Match(
                Anno(
                  Op("ListCompr", [Var("g_36"), Var("h_36")])
                , Var("i_36")
                )
              )
            , Seq(
                Build(Var("g_36"))
              , Seq(
                  CallT(SVar("n_22"), [], [])
                , Seq(
                    Match(Var("j_36"))
                  , Seq(
                      Build(Var("h_36"))
                    , Seq(
                        CallT(SVar("o_22"), [], [])
                      , Seq(
                          Match(Var("k_36"))
                        , Build(
                            Anno(
                              Op("ListCompr", [Var("j_36"), Var("k_36")])
                            , Var("i_36")
                            )
                          )
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
              "p_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["o_36", "l_36", "m_36", "n_36", "p_36", "q_36", "r_36"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ListFirstFromTo"
                  , [Var("l_36"), Var("m_36"), Var("n_36")]
                  )
                , Var("o_36")
                )
              )
            , Seq(
                Build(Var("l_36"))
              , Seq(
                  CallT(SVar("p_22"), [], [])
                , Seq(
                    Match(Var("p_36"))
                  , Seq(
                      Build(Var("m_36"))
                    , Seq(
                        CallT(SVar("q_22"), [], [])
                      , Seq(
                          Match(Var("q_36"))
                        , Seq(
                            Build(Var("n_36"))
                          , Seq(
                              CallT(SVar("r_22"), [], [])
                            , Seq(
                                Match(Var("r_36"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ListFirstFromTo"
                                    , [Var("p_36"), Var("q_36"), Var("r_36")]
                                    )
                                  , Var("o_36")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["u_36", "s_36", "t_36", "v_36", "w_36"]
          , Seq(
              Match(
                Anno(
                  Op("ListFromTo", [Var("s_36"), Var("t_36")])
                , Var("u_36")
                )
              )
            , Seq(
                Build(Var("s_36"))
              , Seq(
                  CallT(SVar("s_22"), [], [])
                , Seq(
                    Match(Var("v_36"))
                  , Seq(
                      Build(Var("t_36"))
                    , Seq(
                        CallT(SVar("t_22"), [], [])
                      , Seq(
                          Match(Var("w_36"))
                        , Build(
                            Anno(
                              Op("ListFromTo", [Var("v_36"), Var("w_36")])
                            , Var("u_36")
                            )
                          )
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
              "u_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_36", "x_36", "y_36", "a_37", "b_37"]
          , Seq(
              Match(
                Anno(
                  Op("ListFirstFrom", [Var("x_36"), Var("y_36")])
                , Var("z_36")
                )
              )
            , Seq(
                Build(Var("x_36"))
              , Seq(
                  CallT(SVar("u_22"), [], [])
                , Seq(
                    Match(Var("a_37"))
                  , Seq(
                      Build(Var("y_36"))
                    , Seq(
                        CallT(SVar("v_22"), [], [])
                      , Seq(
                          Match(Var("b_37"))
                        , Build(
                            Anno(
                              Op("ListFirstFrom", [Var("a_37"), Var("b_37")])
                            , Var("z_36")
                            )
                          )
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
              "w_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_37", "c_37", "e_37"]
          , Seq(
              Match(
                Anno(Op("ListFrom", [Var("c_37")]), Var("d_37"))
              )
            , Seq(
                Build(Var("c_37"))
              , Seq(
                  CallT(SVar("w_22"), [], [])
                , Seq(
                    Match(Var("e_37"))
                  , Build(
                      Anno(Op("ListFrom", [Var("e_37")]), Var("d_37"))
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
              "x_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_37", "f_37", "h_37"]
          , Seq(
              Match(
                Anno(Op("List", [Var("f_37")]), Var("g_37"))
              )
            , Seq(
                Build(Var("f_37"))
              , Seq(
                  CallT(SVar("x_22"), [], [])
                , Seq(
                    Match(Var("h_37"))
                  , Build(
                      Anno(Op("List", [Var("h_37")]), Var("g_37"))
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
              "y_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_37", "i_37", "k_37"]
          , Seq(
              Match(
                Anno(Op("QualLet", [Var("i_37")]), Var("j_37"))
              )
            , Seq(
                Build(Var("i_37"))
              , Seq(
                  CallT(SVar("y_22"), [], [])
                , Seq(
                    Match(Var("k_37"))
                  , Build(
                      Anno(Op("QualLet", [Var("k_37")]), Var("j_37"))
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
              "z_22"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "a_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_37", "l_37", "m_37", "o_37", "p_37"]
          , Seq(
              Match(
                Anno(
                  Op("QualBind", [Var("l_37"), Var("m_37")])
                , Var("n_37")
                )
              )
            , Seq(
                Build(Var("l_37"))
              , Seq(
                  CallT(SVar("z_22"), [], [])
                , Seq(
                    Match(Var("o_37"))
                  , Seq(
                      Build(Var("m_37"))
                    , Seq(
                        CallT(SVar("a_23"), [], [])
                      , Seq(
                          Match(Var("p_37"))
                        , Build(
                            Anno(
                              Op("QualBind", [Var("o_37"), Var("p_37")])
                            , Var("n_37")
                            )
                          )
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
              "b_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "c_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_37", "q_37", "r_37", "t_37", "u_37"]
          , Seq(
              Match(
                Anno(
                  Op("PatBind", [Var("q_37"), Var("r_37")])
                , Var("s_37")
                )
              )
            , Seq(
                Build(Var("q_37"))
              , Seq(
                  CallT(SVar("b_23"), [], [])
                , Seq(
                    Match(Var("t_37"))
                  , Seq(
                      Build(Var("r_37"))
                    , Seq(
                        CallT(SVar("c_23"), [], [])
                      , Seq(
                          Match(Var("u_37"))
                        , Build(
                            Anno(
                              Op("PatBind", [Var("t_37"), Var("u_37")])
                            , Var("s_37")
                            )
                          )
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
              "d_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_37", "v_37", "x_37"]
          , Seq(
              Match(
                Anno(Op("LabeledPats", [Var("v_37")]), Var("w_37"))
              )
            , Seq(
                Build(Var("v_37"))
              , Seq(
                  CallT(SVar("d_23"), [], [])
                , Seq(
                    Match(Var("x_37"))
                  , Build(
                      Anno(Op("LabeledPats", [Var("x_37")]), Var("w_37"))
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
              "e_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_37", "y_37", "a_38"]
          , Seq(
              Match(
                Anno(Op("Irrefutable", [Var("y_37")]), Var("z_37"))
              )
            , Seq(
                Build(Var("y_37"))
              , Seq(
                  CallT(SVar("e_23"), [], [])
                , Seq(
                    Match(Var("a_38"))
                  , Build(
                      Anno(Op("Irrefutable", [Var("a_38")]), Var("z_37"))
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
              "f_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_38", "b_38", "d_38"]
          , Seq(
              Match(
                Anno(Op("ListPat", [Var("b_38")]), Var("c_38"))
              )
            , Seq(
                Build(Var("b_38"))
              , Seq(
                  CallT(SVar("f_23"), [], [])
                , Seq(
                    Match(Var("d_38"))
                  , Build(
                      Anno(Op("ListPat", [Var("d_38")]), Var("c_38"))
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
              "g_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "h_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_38", "e_38", "f_38", "h_38", "i_38"]
          , Seq(
              Match(
                Anno(
                  Op("TuplePat", [Var("e_38"), Var("f_38")])
                , Var("g_38")
                )
              )
            , Seq(
                Build(Var("e_38"))
              , Seq(
                  CallT(SVar("g_23"), [], [])
                , Seq(
                    Match(Var("h_38"))
                  , Seq(
                      Build(Var("f_38"))
                    , Seq(
                        CallT(SVar("h_23"), [], [])
                      , Seq(
                          Match(Var("i_38"))
                        , Build(
                            Anno(
                              Op("TuplePat", [Var("h_38"), Var("i_38")])
                            , Var("g_38")
                            )
                          )
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
              "i_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_38", "j_38", "k_38", "m_38", "n_38"]
          , Seq(
              Match(
                Anno(
                  Op("LabeledPat", [Var("j_38"), Var("k_38")])
                , Var("l_38")
                )
              )
            , Seq(
                Build(Var("j_38"))
              , Seq(
                  CallT(SVar("i_23"), [], [])
                , Seq(
                    Match(Var("m_38"))
                  , Seq(
                      Build(Var("k_38"))
                    , Seq(
                        CallT(SVar("j_23"), [], [])
                      , Seq(
                          Match(Var("n_38"))
                        , Build(
                            Anno(
                              Op("LabeledPat", [Var("m_38"), Var("n_38")])
                            , Var("l_38")
                            )
                          )
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
              "k_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_38", "o_38", "q_38"]
          , Seq(
              Match(
                Anno(Op("ConstrPat", [Var("o_38")]), Var("p_38"))
              )
            , Seq(
                Build(Var("o_38"))
              , Seq(
                  CallT(SVar("k_23"), [], [])
                , Seq(
                    Match(Var("q_38"))
                  , Build(
                      Anno(Op("ConstrPat", [Var("q_38")]), Var("p_38"))
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
            ["t_38", "r_38", "s_38", "u_38", "v_38"]
          , Seq(
              Match(
                Anno(
                  Op("NamedPat", [Var("r_38"), Var("s_38")])
                , Var("t_38")
                )
              )
            , Seq(
                Build(Var("r_38"))
              , Seq(
                  CallT(SVar("l_23"), [], [])
                , Seq(
                    Match(Var("u_38"))
                  , Seq(
                      Build(Var("s_38"))
                    , Seq(
                        CallT(SVar("m_23"), [], [])
                      , Seq(
                          Match(Var("v_38"))
                        , Build(
                            Anno(
                              Op("NamedPat", [Var("u_38"), Var("v_38")])
                            , Var("t_38")
                            )
                          )
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
              "n_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_38", "w_38", "x_38", "z_38", "a_39"]
          , Seq(
              Match(
                Anno(
                  Op("ConstrApp", [Var("w_38"), Var("x_38")])
                , Var("y_38")
                )
              )
            , Seq(
                Build(Var("w_38"))
              , Seq(
                  CallT(SVar("n_23"), [], [])
                , Seq(
                    Match(Var("z_38"))
                  , Seq(
                      Build(Var("x_38"))
                    , Seq(
                        CallT(SVar("o_23"), [], [])
                      , Seq(
                          Match(Var("a_39"))
                        , Build(
                            Anno(
                              Op("ConstrApp", [Var("z_38"), Var("a_39")])
                            , Var("y_38")
                            )
                          )
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
              "p_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_39", "b_39", "d_39"]
          , Seq(
              Match(
                Anno(Op("NegationPat", [Var("b_39")]), Var("c_39"))
              )
            , Seq(
                Build(Var("b_39"))
              , Seq(
                  CallT(SVar("p_23"), [], [])
                , Seq(
                    Match(Var("d_39"))
                  , Build(
                      Anno(Op("NegationPat", [Var("d_39")]), Var("c_39"))
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
          , VarDec(
              "s_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_39", "e_39", "f_39", "g_39", "i_39", "j_39", "k_39"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "BinOpApp"
                  , [Var("e_39"), Var("f_39"), Var("g_39")]
                  )
                , Var("h_39")
                )
              )
            , Seq(
                Build(Var("e_39"))
              , Seq(
                  CallT(SVar("q_23"), [], [])
                , Seq(
                    Match(Var("i_39"))
                  , Seq(
                      Build(Var("f_39"))
                    , Seq(
                        CallT(SVar("r_23"), [], [])
                      , Seq(
                          Match(Var("j_39"))
                        , Seq(
                            Build(Var("g_39"))
                          , Seq(
                              CallT(SVar("s_23"), [], [])
                            , Seq(
                                Match(Var("k_39"))
                              , Build(
                                  Anno(
                                    Op(
                                      "BinOpApp"
                                    , [Var("i_39"), Var("j_39"), Var("k_39")]
                                    )
                                  , Var("h_39")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["n_39", "l_39", "m_39", "o_39", "p_39"]
          , Seq(
              Match(
                Anno(
                  Op("DeclSeq", [Var("l_39"), Var("m_39")])
                , Var("n_39")
                )
              )
            , Seq(
                Build(Var("l_39"))
              , Seq(
                  CallT(SVar("t_23"), [], [])
                , Seq(
                    Match(Var("o_39"))
                  , Seq(
                      Build(Var("m_39"))
                    , Seq(
                        CallT(SVar("u_23"), [], [])
                      , Seq(
                          Match(Var("p_39"))
                        , Build(
                            Anno(
                              Op("DeclSeq", [Var("o_39"), Var("p_39")])
                            , Var("n_39")
                            )
                          )
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
              "v_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_39", "q_39", "s_39"]
          , Seq(
              Match(
                Anno(Op("DeclList", [Var("q_39")]), Var("r_39"))
              )
            , Seq(
                Build(Var("q_39"))
              , Seq(
                  CallT(SVar("v_23"), [], [])
                , Seq(
                    Match(Var("s_39"))
                  , Build(
                      Anno(Op("DeclList", [Var("s_39")]), Var("r_39"))
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
              "w_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_39", "t_39", "v_39"]
          , Seq(
              Match(
                Anno(Op("Where", [Var("t_39")]), Var("u_39"))
              )
            , Seq(
                Build(Var("t_39"))
              , Seq(
                  CallT(SVar("w_23"), [], [])
                , Seq(
                    Match(Var("v_39"))
                  , Build(
                      Anno(Op("Where", [Var("v_39")]), Var("u_39"))
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
              "x_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_39", "w_39", "x_39", "z_39", "a_40"]
          , Seq(
              Match(
                Anno(
                  Op("NestedFunLHS", [Var("w_39"), Var("x_39")])
                , Var("y_39")
                )
              )
            , Seq(
                Build(Var("w_39"))
              , Seq(
                  CallT(SVar("x_23"), [], [])
                , Seq(
                    Match(Var("z_39"))
                  , Seq(
                      Build(Var("x_39"))
                    , Seq(
                        CallT(SVar("y_23"), [], [])
                      , Seq(
                          Match(Var("a_40"))
                        , Build(
                            Anno(
                              Op("NestedFunLHS", [Var("z_39"), Var("a_40")])
                            , Var("y_39")
                            )
                          )
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
              "z_23"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["e_40", "b_40", "c_40", "d_40", "f_40", "g_40", "h_40"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "OpFunLHS"
                  , [Var("b_40"), Var("c_40"), Var("d_40")]
                  )
                , Var("e_40")
                )
              )
            , Seq(
                Build(Var("b_40"))
              , Seq(
                  CallT(SVar("z_23"), [], [])
                , Seq(
                    Match(Var("f_40"))
                  , Seq(
                      Build(Var("c_40"))
                    , Seq(
                        CallT(SVar("a_24"), [], [])
                      , Seq(
                          Match(Var("g_40"))
                        , Seq(
                            Build(Var("d_40"))
                          , Seq(
                              CallT(SVar("b_24"), [], [])
                            , Seq(
                                Match(Var("h_40"))
                              , Build(
                                  Anno(
                                    Op(
                                      "OpFunLHS"
                                    , [Var("f_40"), Var("g_40"), Var("h_40")]
                                    )
                                  , Var("e_40")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["k_40", "i_40", "j_40", "l_40", "m_40"]
          , Seq(
              Match(
                Anno(
                  Op("VarFunLHS", [Var("i_40"), Var("j_40")])
                , Var("k_40")
                )
              )
            , Seq(
                Build(Var("i_40"))
              , Seq(
                  CallT(SVar("c_24"), [], [])
                , Seq(
                    Match(Var("l_40"))
                  , Seq(
                      Build(Var("j_40"))
                    , Seq(
                        CallT(SVar("d_24"), [], [])
                      , Seq(
                          Match(Var("m_40"))
                        , Build(
                            Anno(
                              Op("VarFunLHS", [Var("l_40"), Var("m_40")])
                            , Var("k_40")
                            )
                          )
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
              "e_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_40", "n_40", "o_40", "q_40", "r_40"]
          , Seq(
              Match(
                Anno(
                  Op("Guarded", [Var("n_40"), Var("o_40")])
                , Var("p_40")
                )
              )
            , Seq(
                Build(Var("n_40"))
              , Seq(
                  CallT(SVar("e_24"), [], [])
                , Seq(
                    Match(Var("q_40"))
                  , Seq(
                      Build(Var("o_40"))
                    , Seq(
                        CallT(SVar("f_24"), [], [])
                      , Seq(
                          Match(Var("r_40"))
                        , Build(
                            Anno(
                              Op("Guarded", [Var("q_40"), Var("r_40")])
                            , Var("p_40")
                            )
                          )
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
              "g_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["v_40", "s_40", "t_40", "u_40", "w_40", "x_40", "y_40"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "GdValdef"
                  , [Var("s_40"), Var("t_40"), Var("u_40")]
                  )
                , Var("v_40")
                )
              )
            , Seq(
                Build(Var("s_40"))
              , Seq(
                  CallT(SVar("g_24"), [], [])
                , Seq(
                    Match(Var("w_40"))
                  , Seq(
                      Build(Var("t_40"))
                    , Seq(
                        CallT(SVar("h_24"), [], [])
                      , Seq(
                          Match(Var("x_40"))
                        , Seq(
                            Build(Var("u_40"))
                          , Seq(
                              CallT(SVar("i_24"), [], [])
                            , Seq(
                                Match(Var("y_40"))
                              , Build(
                                  Anno(
                                    Op(
                                      "GdValdef"
                                    , [Var("w_40"), Var("x_40"), Var("y_40")]
                                    )
                                  , Var("v_40")
                                  )
                                )
                              )
                            )
                          )
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
            ["c_41", "z_40", "a_41", "b_41", "d_41", "e_41", "f_41"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Valdef"
                  , [Var("z_40"), Var("a_41"), Var("b_41")]
                  )
                , Var("c_41")
                )
              )
            , Seq(
                Build(Var("z_40"))
              , Seq(
                  CallT(SVar("j_24"), [], [])
                , Seq(
                    Match(Var("d_41"))
                  , Seq(
                      Build(Var("a_41"))
                    , Seq(
                        CallT(SVar("k_24"), [], [])
                      , Seq(
                          Match(Var("e_41"))
                        , Seq(
                            Build(Var("b_41"))
                          , Seq(
                              CallT(SVar("l_24"), [], [])
                            , Seq(
                                Match(Var("f_41"))
                              , Build(
                                  Anno(
                                    Op(
                                      "Valdef"
                                    , [Var("d_41"), Var("e_41"), Var("f_41")]
                                    )
                                  , Var("c_41")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["i_41", "g_41", "h_41", "j_41", "k_41"]
          , Seq(
              Match(
                Anno(
                  Op("AltSeq", [Var("g_41"), Var("h_41")])
                , Var("i_41")
                )
              )
            , Seq(
                Build(Var("g_41"))
              , Seq(
                  CallT(SVar("m_24"), [], [])
                , Seq(
                    Match(Var("j_41"))
                  , Seq(
                      Build(Var("h_41"))
                    , Seq(
                        CallT(SVar("n_24"), [], [])
                      , Seq(
                          Match(Var("k_41"))
                        , Build(
                            Anno(
                              Op("AltSeq", [Var("j_41"), Var("k_41")])
                            , Var("i_41")
                            )
                          )
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
              "o_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_41", "l_41", "n_41"]
          , Seq(
              Match(
                Anno(Op("AltList", [Var("l_41")]), Var("m_41"))
              )
            , Seq(
                Build(Var("l_41"))
              , Seq(
                  CallT(SVar("o_24"), [], [])
                , Seq(
                    Match(Var("n_41"))
                  , Build(
                      Anno(Op("AltList", [Var("n_41")]), Var("m_41"))
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
            ["q_41", "o_41", "p_41", "r_41", "s_41"]
          , Seq(
              Match(
                Anno(
                  Op("GdPat", [Var("o_41"), Var("p_41")])
                , Var("q_41")
                )
              )
            , Seq(
                Build(Var("o_41"))
              , Seq(
                  CallT(SVar("p_24"), [], [])
                , Seq(
                    Match(Var("r_41"))
                  , Seq(
                      Build(Var("p_41"))
                    , Seq(
                        CallT(SVar("q_24"), [], [])
                      , Seq(
                          Match(Var("s_41"))
                        , Build(
                            Anno(
                              Op("GdPat", [Var("r_41"), Var("s_41")])
                            , Var("q_41")
                            )
                          )
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
              "r_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["w_41", "t_41", "u_41", "v_41", "x_41", "y_41", "z_41"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "GdAlt"
                  , [Var("t_41"), Var("u_41"), Var("v_41")]
                  )
                , Var("w_41")
                )
              )
            , Seq(
                Build(Var("t_41"))
              , Seq(
                  CallT(SVar("r_24"), [], [])
                , Seq(
                    Match(Var("x_41"))
                  , Seq(
                      Build(Var("u_41"))
                    , Seq(
                        CallT(SVar("s_24"), [], [])
                      , Seq(
                          Match(Var("y_41"))
                        , Seq(
                            Build(Var("v_41"))
                          , Seq(
                              CallT(SVar("t_24"), [], [])
                            , Seq(
                                Match(Var("z_41"))
                              , Build(
                                  Anno(
                                    Op(
                                      "GdAlt"
                                    , [Var("x_41"), Var("y_41"), Var("z_41")]
                                    )
                                  , Var("w_41")
                                  )
                                )
                              )
                            )
                          )
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
            ["d_42", "a_42", "b_42", "c_42", "e_42", "f_42", "g_42"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Alt"
                  , [Var("a_42"), Var("b_42"), Var("c_42")]
                  )
                , Var("d_42")
                )
              )
            , Seq(
                Build(Var("a_42"))
              , Seq(
                  CallT(SVar("u_24"), [], [])
                , Seq(
                    Match(Var("e_42"))
                  , Seq(
                      Build(Var("b_42"))
                    , Seq(
                        CallT(SVar("v_24"), [], [])
                      , Seq(
                          Match(Var("f_42"))
                        , Seq(
                            Build(Var("c_42"))
                          , Seq(
                              CallT(SVar("w_24"), [], [])
                            , Seq(
                                Match(Var("g_42"))
                              , Build(
                                  Anno(
                                    Op(
                                      "Alt"
                                    , [Var("e_42"), Var("f_42"), Var("g_42")]
                                    )
                                  , Var("d_42")
                                  )
                                )
                              )
                            )
                          )
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
              "x_24"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_42", "h_42", "j_42"]
          , Seq(
              Match(
                Anno(Op("LabelBinds", [Var("h_42")]), Var("i_42"))
              )
            , Seq(
                Build(Var("h_42"))
              , Seq(
                  CallT(SVar("x_24"), [], [])
                , Seq(
                    Match(Var("j_42"))
                  , Build(
                      Anno(Op("LabelBinds", [Var("j_42")]), Var("i_42"))
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
          , VarDec(
              "a_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_42", "k_42", "l_42", "m_42", "o_42", "p_42", "q_42"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "FixDecl"
                  , [Var("k_42"), Var("l_42"), Var("m_42")]
                  )
                , Var("n_42")
                )
              )
            , Seq(
                Build(Var("k_42"))
              , Seq(
                  CallT(SVar("y_24"), [], [])
                , Seq(
                    Match(Var("o_42"))
                  , Seq(
                      Build(Var("l_42"))
                    , Seq(
                        CallT(SVar("z_24"), [], [])
                      , Seq(
                          Match(Var("p_42"))
                        , Seq(
                            Build(Var("m_42"))
                          , Seq(
                              CallT(SVar("a_25"), [], [])
                            , Seq(
                                Match(Var("q_42"))
                              , Build(
                                  Anno(
                                    Op(
                                      "FixDecl"
                                    , [Var("o_42"), Var("p_42"), Var("q_42")]
                                    )
                                  , Var("n_42")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["t_42", "r_42", "s_42", "u_42", "v_42"]
          , Seq(
              Match(
                Anno(
                  Op("ECons", [Var("r_42"), Var("s_42")])
                , Var("t_42")
                )
              )
            , Seq(
                Build(Var("r_42"))
              , Seq(
                  CallT(SVar("b_25"), [], [])
                , Seq(
                    Match(Var("u_42"))
                  , Seq(
                      Build(Var("s_42"))
                    , Seq(
                        CallT(SVar("c_25"), [], [])
                      , Seq(
                          Match(Var("v_42"))
                        , Build(
                            Anno(
                              Op("ECons", [Var("u_42"), Var("v_42")])
                            , Var("t_42")
                            )
                          )
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
              "d_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["z_42", "w_42", "x_42", "y_42", "a_43", "b_43", "c_43"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ArrOpApp"
                  , [Var("w_42"), Var("x_42"), Var("y_42")]
                  )
                , Var("z_42")
                )
              )
            , Seq(
                Build(Var("w_42"))
              , Seq(
                  CallT(SVar("d_25"), [], [])
                , Seq(
                    Match(Var("a_43"))
                  , Seq(
                      Build(Var("x_42"))
                    , Seq(
                        CallT(SVar("e_25"), [], [])
                      , Seq(
                          Match(Var("b_43"))
                        , Seq(
                            Build(Var("y_42"))
                          , Seq(
                              CallT(SVar("f_25"), [], [])
                            , Seq(
                                Match(Var("c_43"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ArrOpApp"
                                    , [Var("a_43"), Var("b_43"), Var("c_43")]
                                    )
                                  , Var("z_42")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["f_43", "d_43", "e_43", "g_43", "h_43"]
          , Seq(
              Match(
                Anno(
                  Op("ArrForm", [Var("d_43"), Var("e_43")])
                , Var("f_43")
                )
              )
            , Seq(
                Build(Var("d_43"))
              , Seq(
                  CallT(SVar("g_25"), [], [])
                , Seq(
                    Match(Var("g_43"))
                  , Seq(
                      Build(Var("e_43"))
                    , Seq(
                        CallT(SVar("h_25"), [], [])
                      , Seq(
                          Match(Var("h_43"))
                        , Build(
                            Anno(
                              Op("ArrForm", [Var("g_43"), Var("h_43")])
                            , Var("f_43")
                            )
                          )
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
              "i_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_43", "i_43", "j_43", "l_43", "m_43"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAppBin", [Var("i_43"), Var("j_43")])
                , Var("k_43")
                )
              )
            , Seq(
                Build(Var("i_43"))
              , Seq(
                  CallT(SVar("i_25"), [], [])
                , Seq(
                    Match(Var("l_43"))
                  , Seq(
                      Build(Var("j_43"))
                    , Seq(
                        CallT(SVar("j_25"), [], [])
                      , Seq(
                          Match(Var("m_43"))
                        , Build(
                            Anno(
                              Op("ArrAppBin", [Var("l_43"), Var("m_43")])
                            , Var("k_43")
                            )
                          )
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
              "k_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_43", "n_43", "p_43"]
          , Seq(
              Match(
                Anno(Op("ArrDo", [Var("n_43")]), Var("o_43"))
              )
            , Seq(
                Build(Var("n_43"))
              , Seq(
                  CallT(SVar("k_25"), [], [])
                , Seq(
                    Match(Var("p_43"))
                  , Build(
                      Anno(Op("ArrDo", [Var("p_43")]), Var("o_43"))
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
            ["s_43", "q_43", "r_43", "t_43", "u_43"]
          , Seq(
              Match(
                Anno(
                  Op("ArrCase", [Var("q_43"), Var("r_43")])
                , Var("s_43")
                )
              )
            , Seq(
                Build(Var("q_43"))
              , Seq(
                  CallT(SVar("l_25"), [], [])
                , Seq(
                    Match(Var("t_43"))
                  , Seq(
                      Build(Var("r_43"))
                    , Seq(
                        CallT(SVar("m_25"), [], [])
                      , Seq(
                          Match(Var("u_43"))
                        , Build(
                            Anno(
                              Op("ArrCase", [Var("t_43"), Var("u_43")])
                            , Var("s_43")
                            )
                          )
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
              "n_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["y_43", "v_43", "w_43", "x_43", "z_43", "a_44", "b_44"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ArrIf"
                  , [Var("v_43"), Var("w_43"), Var("x_43")]
                  )
                , Var("y_43")
                )
              )
            , Seq(
                Build(Var("v_43"))
              , Seq(
                  CallT(SVar("n_25"), [], [])
                , Seq(
                    Match(Var("z_43"))
                  , Seq(
                      Build(Var("w_43"))
                    , Seq(
                        CallT(SVar("o_25"), [], [])
                      , Seq(
                          Match(Var("a_44"))
                        , Seq(
                            Build(Var("x_43"))
                          , Seq(
                              CallT(SVar("p_25"), [], [])
                            , Seq(
                                Match(Var("b_44"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ArrIf"
                                    , [Var("z_43"), Var("a_44"), Var("b_44")]
                                    )
                                  , Var("y_43")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["e_44", "c_44", "d_44", "f_44", "g_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrLet", [Var("c_44"), Var("d_44")])
                , Var("e_44")
                )
              )
            , Seq(
                Build(Var("c_44"))
              , Seq(
                  CallT(SVar("q_25"), [], [])
                , Seq(
                    Match(Var("f_44"))
                  , Seq(
                      Build(Var("d_44"))
                    , Seq(
                        CallT(SVar("r_25"), [], [])
                      , Seq(
                          Match(Var("g_44"))
                        , Build(
                            Anno(
                              Op("ArrLet", [Var("f_44"), Var("g_44")])
                            , Var("e_44")
                            )
                          )
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
              "s_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_44", "h_44", "i_44", "k_44", "l_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAbs", [Var("h_44"), Var("i_44")])
                , Var("j_44")
                )
              )
            , Seq(
                Build(Var("h_44"))
              , Seq(
                  CallT(SVar("s_25"), [], [])
                , Seq(
                    Match(Var("k_44"))
                  , Seq(
                      Build(Var("i_44"))
                    , Seq(
                        CallT(SVar("t_25"), [], [])
                      , Seq(
                          Match(Var("l_44"))
                        , Build(
                            Anno(
                              Op("ArrAbs", [Var("k_44"), Var("l_44")])
                            , Var("j_44")
                            )
                          )
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
              "u_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_44", "m_44", "n_44", "p_44", "q_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrHigher", [Var("m_44"), Var("n_44")])
                , Var("o_44")
                )
              )
            , Seq(
                Build(Var("m_44"))
              , Seq(
                  CallT(SVar("u_25"), [], [])
                , Seq(
                    Match(Var("p_44"))
                  , Seq(
                      Build(Var("n_44"))
                    , Seq(
                        CallT(SVar("v_25"), [], [])
                      , Seq(
                          Match(Var("q_44"))
                        , Build(
                            Anno(
                              Op("ArrHigher", [Var("p_44"), Var("q_44")])
                            , Var("o_44")
                            )
                          )
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
              "w_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_44", "r_44", "s_44", "u_44", "v_44"]
          , Seq(
              Match(
                Anno(
                  Op("ArrFirst", [Var("r_44"), Var("s_44")])
                , Var("t_44")
                )
              )
            , Seq(
                Build(Var("r_44"))
              , Seq(
                  CallT(SVar("w_25"), [], [])
                , Seq(
                    Match(Var("u_44"))
                  , Seq(
                      Build(Var("s_44"))
                    , Seq(
                        CallT(SVar("x_25"), [], [])
                      , Seq(
                          Match(Var("v_44"))
                        , Build(
                            Anno(
                              Op("ArrFirst", [Var("u_44"), Var("v_44")])
                            , Var("t_44")
                            )
                          )
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
              "y_25"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["z_44", "w_44", "x_44", "y_44", "a_45", "b_45", "c_45"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Typed"
                  , [Var("w_44"), Var("x_44"), Var("y_44")]
                  )
                , Var("z_44")
                )
              )
            , Seq(
                Build(Var("w_44"))
              , Seq(
                  CallT(SVar("y_25"), [], [])
                , Seq(
                    Match(Var("a_45"))
                  , Seq(
                      Build(Var("x_44"))
                    , Seq(
                        CallT(SVar("z_25"), [], [])
                      , Seq(
                          Match(Var("b_45"))
                        , Seq(
                            Build(Var("y_44"))
                          , Seq(
                              CallT(SVar("a_26"), [], [])
                            , Seq(
                                Match(Var("c_45"))
                              , Build(
                                  Anno(
                                    Op(
                                      "Typed"
                                    , [Var("a_45"), Var("b_45"), Var("c_45")]
                                    )
                                  , Var("z_44")
                                  )
                                )
                              )
                            )
                          )
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
              "b_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_45", "d_45", "f_45"]
          , Seq(
              Match(
                Anno(Op("Negation", [Var("d_45")]), Var("e_45"))
              )
            , Seq(
                Build(Var("d_45"))
              , Seq(
                  CallT(SVar("b_26"), [], [])
                , Seq(
                    Match(Var("f_45"))
                  , Build(
                      Anno(Op("Negation", [Var("f_45")]), Var("e_45"))
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
            ["i_45", "g_45", "h_45", "j_45", "k_45"]
          , Seq(
              Match(
                Anno(
                  Op("Labeled", [Var("g_45"), Var("h_45")])
                , Var("i_45")
                )
              )
            , Seq(
                Build(Var("g_45"))
              , Seq(
                  CallT(SVar("c_26"), [], [])
                , Seq(
                    Match(Var("j_45"))
                  , Seq(
                      Build(Var("h_45"))
                    , Seq(
                        CallT(SVar("d_26"), [], [])
                      , Seq(
                          Match(Var("k_45"))
                        , Build(
                            Anno(
                              Op("Labeled", [Var("j_45"), Var("k_45")])
                            , Var("i_45")
                            )
                          )
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
              "e_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_45", "l_45", "m_45", "o_45", "p_45"]
          , Seq(
              Match(
                Anno(
                  Op("Named", [Var("l_45"), Var("m_45")])
                , Var("n_45")
                )
              )
            , Seq(
                Build(Var("l_45"))
              , Seq(
                  CallT(SVar("e_26"), [], [])
                , Seq(
                    Match(Var("o_45"))
                  , Seq(
                      Build(Var("m_45"))
                    , Seq(
                        CallT(SVar("f_26"), [], [])
                      , Seq(
                          Match(Var("p_45"))
                        , Build(
                            Anno(
                              Op("Named", [Var("o_45"), Var("p_45")])
                            , Var("n_45")
                            )
                          )
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
              "g_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
            ["t_45", "q_45", "r_45", "s_45", "u_45", "v_45", "w_45"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "OpApp"
                  , [Var("q_45"), Var("r_45"), Var("s_45")]
                  )
                , Var("t_45")
                )
              )
            , Seq(
                Build(Var("q_45"))
              , Seq(
                  CallT(SVar("g_26"), [], [])
                , Seq(
                    Match(Var("u_45"))
                  , Seq(
                      Build(Var("r_45"))
                    , Seq(
                        CallT(SVar("h_26"), [], [])
                      , Seq(
                          Match(Var("v_45"))
                        , Seq(
                            Build(Var("s_45"))
                          , Seq(
                              CallT(SVar("i_26"), [], [])
                            , Seq(
                                Match(Var("w_45"))
                              , Build(
                                  Anno(
                                    Op(
                                      "OpApp"
                                    , [Var("u_45"), Var("v_45"), Var("w_45")]
                                    )
                                  , Var("t_45")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["z_45", "x_45", "y_45", "a_46", "b_46"]
          , Seq(
              Match(
                Anno(
                  Op("AppBin", [Var("x_45"), Var("y_45")])
                , Var("z_45")
                )
              )
            , Seq(
                Build(Var("x_45"))
              , Seq(
                  CallT(SVar("j_26"), [], [])
                , Seq(
                    Match(Var("a_46"))
                  , Seq(
                      Build(Var("y_45"))
                    , Seq(
                        CallT(SVar("k_26"), [], [])
                      , Seq(
                          Match(Var("b_46"))
                        , Build(
                            Anno(
                              Op("AppBin", [Var("a_46"), Var("b_46")])
                            , Var("z_45")
                            )
                          )
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
              "l_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_46", "c_46", "d_46", "f_46", "g_46"]
          , Seq(
              Match(
                Anno(
                  Op("Case", [Var("c_46"), Var("d_46")])
                , Var("e_46")
                )
              )
            , Seq(
                Build(Var("c_46"))
              , Seq(
                  CallT(SVar("l_26"), [], [])
                , Seq(
                    Match(Var("f_46"))
                  , Seq(
                      Build(Var("d_46"))
                    , Seq(
                        CallT(SVar("m_26"), [], [])
                      , Seq(
                          Match(Var("g_46"))
                        , Build(
                            Anno(
                              Op("Case", [Var("f_46"), Var("g_46")])
                            , Var("e_46")
                            )
                          )
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
              "n_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_46", "h_46", "j_46"]
          , Seq(
              Match(
                Anno(Op("Do", [Var("h_46")]), Var("i_46"))
              )
            , Seq(
                Build(Var("h_46"))
              , Seq(
                  CallT(SVar("n_26"), [], [])
                , Seq(
                    Match(Var("j_46"))
                  , Build(
                      Anno(Op("Do", [Var("j_46")]), Var("i_46"))
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
          , VarDec(
              "q_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_46", "k_46", "l_46", "m_46", "o_46", "p_46", "q_46"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "If"
                  , [Var("k_46"), Var("l_46"), Var("m_46")]
                  )
                , Var("n_46")
                )
              )
            , Seq(
                Build(Var("k_46"))
              , Seq(
                  CallT(SVar("o_26"), [], [])
                , Seq(
                    Match(Var("o_46"))
                  , Seq(
                      Build(Var("l_46"))
                    , Seq(
                        CallT(SVar("p_26"), [], [])
                      , Seq(
                          Match(Var("p_46"))
                        , Seq(
                            Build(Var("m_46"))
                          , Seq(
                              CallT(SVar("q_26"), [], [])
                            , Seq(
                                Match(Var("q_46"))
                              , Build(
                                  Anno(
                                    Op(
                                      "If"
                                    , [Var("o_46"), Var("p_46"), Var("q_46")]
                                    )
                                  , Var("n_46")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["t_46", "r_46", "s_46", "u_46", "v_46"]
          , Seq(
              Match(
                Anno(
                  Op("Let", [Var("r_46"), Var("s_46")])
                , Var("t_46")
                )
              )
            , Seq(
                Build(Var("r_46"))
              , Seq(
                  CallT(SVar("r_26"), [], [])
                , Seq(
                    Match(Var("u_46"))
                  , Seq(
                      Build(Var("s_46"))
                    , Seq(
                        CallT(SVar("s_26"), [], [])
                      , Seq(
                          Match(Var("v_46"))
                        , Build(
                            Anno(
                              Op("Let", [Var("u_46"), Var("v_46")])
                            , Var("t_46")
                            )
                          )
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
              "t_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_46", "w_46", "x_46", "z_46", "a_47"]
          , Seq(
              Match(
                Anno(
                  Op("Abs", [Var("w_46"), Var("x_46")])
                , Var("y_46")
                )
              )
            , Seq(
                Build(Var("w_46"))
              , Seq(
                  CallT(SVar("t_26"), [], [])
                , Seq(
                    Match(Var("z_46"))
                  , Seq(
                      Build(Var("x_46"))
                    , Seq(
                        CallT(SVar("u_26"), [], [])
                      , Seq(
                          Match(Var("a_47"))
                        , Build(
                            Anno(
                              Op("Abs", [Var("z_46"), Var("a_47")])
                            , Var("y_46")
                            )
                          )
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
              "v_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_47", "b_47", "c_47", "e_47", "f_47"]
          , Seq(
              Match(
                Anno(
                  Op("RSection", [Var("b_47"), Var("c_47")])
                , Var("d_47")
                )
              )
            , Seq(
                Build(Var("b_47"))
              , Seq(
                  CallT(SVar("v_26"), [], [])
                , Seq(
                    Match(Var("e_47"))
                  , Seq(
                      Build(Var("c_47"))
                    , Seq(
                        CallT(SVar("w_26"), [], [])
                      , Seq(
                          Match(Var("f_47"))
                        , Build(
                            Anno(
                              Op("RSection", [Var("e_47"), Var("f_47")])
                            , Var("d_47")
                            )
                          )
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
              "x_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "y_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_47", "g_47", "h_47", "j_47", "k_47"]
          , Seq(
              Match(
                Anno(
                  Op("LSection", [Var("g_47"), Var("h_47")])
                , Var("i_47")
                )
              )
            , Seq(
                Build(Var("g_47"))
              , Seq(
                  CallT(SVar("x_26"), [], [])
                , Seq(
                    Match(Var("j_47"))
                  , Seq(
                      Build(Var("h_47"))
                    , Seq(
                        CallT(SVar("y_26"), [], [])
                      , Seq(
                          Match(Var("k_47"))
                        , Build(
                            Anno(
                              Op("LSection", [Var("j_47"), Var("k_47")])
                            , Var("i_47")
                            )
                          )
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
              "z_26"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_47", "l_47", "n_47"]
          , Seq(
              Match(
                Anno(Op("Product", [Var("l_47")]), Var("m_47"))
              )
            , Seq(
                Build(Var("l_47"))
              , Seq(
                  CallT(SVar("z_26"), [], [])
                , Seq(
                    Match(Var("n_47"))
                  , Build(
                      Anno(Op("Product", [Var("n_47")]), Var("m_47"))
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
              "a_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_47", "o_47", "q_47"]
          , Seq(
              Match(
                Anno(Op("Lit", [Var("o_47")]), Var("p_47"))
              )
            , Seq(
                Build(Var("o_47"))
              , Seq(
                  CallT(SVar("a_27"), [], [])
                , Seq(
                    Match(Var("q_47"))
                  , Build(
                      Anno(Op("Lit", [Var("q_47")]), Var("p_47"))
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
              "b_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_47", "r_47", "t_47"]
          , Seq(
              Match(
                Anno(Op("Constr", [Var("r_47")]), Var("s_47"))
              )
            , Seq(
                Build(Var("r_47"))
              , Seq(
                  CallT(SVar("b_27"), [], [])
                , Seq(
                    Match(Var("t_47"))
                  , Build(
                      Anno(Op("Constr", [Var("t_47")]), Var("s_47"))
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
              "c_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_47", "u_47", "w_47"]
          , Seq(
              Match(
                Anno(Op("Var", [Var("u_47")]), Var("v_47"))
              )
            , Seq(
                Build(Var("u_47"))
              , Seq(
                  CallT(SVar("c_27"), [], [])
                , Seq(
                    Match(Var("w_47"))
                  , Build(
                      Anno(Op("Var", [Var("w_47")]), Var("v_47"))
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
              "d_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_47", "x_47", "y_47", "a_48", "b_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrProcedure", [Var("x_47"), Var("y_47")])
                , Var("z_47")
                )
              )
            , Seq(
                Build(Var("x_47"))
              , Seq(
                  CallT(SVar("d_27"), [], [])
                , Seq(
                    Match(Var("a_48"))
                  , Seq(
                      Build(Var("y_47"))
                    , Seq(
                        CallT(SVar("e_27"), [], [])
                      , Seq(
                          Match(Var("b_48"))
                        , Build(
                            Anno(
                              Op("ArrProcedure", [Var("a_48"), Var("b_48")])
                            , Var("z_47")
                            )
                          )
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
              "f_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_48", "c_48", "d_48", "f_48", "g_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrStmtSeq", [Var("c_48"), Var("d_48")])
                , Var("e_48")
                )
              )
            , Seq(
                Build(Var("c_48"))
              , Seq(
                  CallT(SVar("f_27"), [], [])
                , Seq(
                    Match(Var("f_48"))
                  , Seq(
                      Build(Var("d_48"))
                    , Seq(
                        CallT(SVar("g_27"), [], [])
                      , Seq(
                          Match(Var("g_48"))
                        , Build(
                            Anno(
                              Op("ArrStmtSeq", [Var("f_48"), Var("g_48")])
                            , Var("e_48")
                            )
                          )
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
              "h_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_48", "h_48", "j_48"]
          , Seq(
              Match(
                Anno(Op("ArrStmtList", [Var("h_48")]), Var("i_48"))
              )
            , Seq(
                Build(Var("h_48"))
              , Seq(
                  CallT(SVar("h_27"), [], [])
                , Seq(
                    Match(Var("j_48"))
                  , Build(
                      Anno(Op("ArrStmtList", [Var("j_48")]), Var("i_48"))
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
              "i_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_48", "k_48", "m_48"]
          , Seq(
              Match(
                Anno(Op("ArrCmdStmt", [Var("k_48")]), Var("l_48"))
              )
            , Seq(
                Build(Var("k_48"))
              , Seq(
                  CallT(SVar("i_27"), [], [])
                , Seq(
                    Match(Var("m_48"))
                  , Build(
                      Anno(Op("ArrCmdStmt", [Var("m_48")]), Var("l_48"))
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
              "j_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_48", "n_48", "o_48", "q_48", "r_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrBindStmt", [Var("n_48"), Var("o_48")])
                , Var("p_48")
                )
              )
            , Seq(
                Build(Var("n_48"))
              , Seq(
                  CallT(SVar("j_27"), [], [])
                , Seq(
                    Match(Var("q_48"))
                  , Seq(
                      Build(Var("o_48"))
                    , Seq(
                        CallT(SVar("k_27"), [], [])
                      , Seq(
                          Match(Var("r_48"))
                        , Build(
                            Anno(
                              Op("ArrBindStmt", [Var("q_48"), Var("r_48")])
                            , Var("p_48")
                            )
                          )
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
              "l_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_48", "s_48", "u_48"]
          , Seq(
              Match(
                Anno(Op("ArrLetStmt", [Var("s_48")]), Var("t_48"))
              )
            , Seq(
                Build(Var("s_48"))
              , Seq(
                  CallT(SVar("l_27"), [], [])
                , Seq(
                    Match(Var("u_48"))
                  , Build(
                      Anno(Op("ArrLetStmt", [Var("u_48")]), Var("t_48"))
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
            ["x_48", "v_48", "w_48", "y_48", "z_48"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAltSeqOff", [Var("v_48"), Var("w_48")])
                , Var("x_48")
                )
              )
            , Seq(
                Build(Var("v_48"))
              , Seq(
                  CallT(SVar("m_27"), [], [])
                , Seq(
                    Match(Var("y_48"))
                  , Seq(
                      Build(Var("w_48"))
                    , Seq(
                        CallT(SVar("n_27"), [], [])
                      , Seq(
                          Match(Var("z_48"))
                        , Build(
                            Anno(
                              Op("ArrAltSeqOff", [Var("y_48"), Var("z_48")])
                            , Var("x_48")
                            )
                          )
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
              "o_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_49", "a_49", "b_49", "d_49", "e_49"]
          , Seq(
              Match(
                Anno(
                  Op("ArrAltSeq", [Var("a_49"), Var("b_49")])
                , Var("c_49")
                )
              )
            , Seq(
                Build(Var("a_49"))
              , Seq(
                  CallT(SVar("o_27"), [], [])
                , Seq(
                    Match(Var("d_49"))
                  , Seq(
                      Build(Var("b_49"))
                    , Seq(
                        CallT(SVar("p_27"), [], [])
                      , Seq(
                          Match(Var("e_49"))
                        , Build(
                            Anno(
                              Op("ArrAltSeq", [Var("d_49"), Var("e_49")])
                            , Var("c_49")
                            )
                          )
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
              "q_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_49", "f_49", "h_49"]
          , Seq(
              Match(
                Anno(Op("ArrAltList", [Var("f_49")]), Var("g_49"))
              )
            , Seq(
                Build(Var("f_49"))
              , Seq(
                  CallT(SVar("q_27"), [], [])
                , Seq(
                    Match(Var("h_49"))
                  , Build(
                      Anno(Op("ArrAltList", [Var("h_49")]), Var("g_49"))
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
          , VarDec(
              "t_27"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_49", "i_49", "j_49", "k_49", "m_49", "n_49", "o_49"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ArrAlt"
                  , [Var("i_49"), Var("j_49"), Var("k_49")]
                  )
                , Var("l_49")
                )
              )
            , Seq(
                Build(Var("i_49"))
              , Seq(
                  CallT(SVar("r_27"), [], [])
                , Seq(
                    Match(Var("m_49"))
                  , Seq(
                      Build(Var("j_49"))
                    , Seq(
                        CallT(SVar("s_27"), [], [])
                      , Seq(
                          Match(Var("n_49"))
                        , Seq(
                            Build(Var("k_49"))
                          , Seq(
                              CallT(SVar("t_27"), [], [])
                            , Seq(
                                Match(Var("o_49"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ArrAlt"
                                    , [Var("m_49"), Var("n_49"), Var("o_49")]
                                    )
                                  , Var("l_49")
                                  )
                                )
                              )
                            )
                          )
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
            ["s_49", "p_49", "q_49", "r_49", "t_49", "u_49", "v_49"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "SignDecl"
                  , [Var("p_49"), Var("q_49"), Var("r_49")]
                  )
                , Var("s_49")
                )
              )
            , Seq(
                Build(Var("p_49"))
              , Seq(
                  CallT(SVar("u_27"), [], [])
                , Seq(
                    Match(Var("t_49"))
                  , Seq(
                      Build(Var("q_49"))
                    , Seq(
                        CallT(SVar("v_27"), [], [])
                      , Seq(
                          Match(Var("u_49"))
                        , Seq(
                            Build(Var("r_49"))
                          , Seq(
                              CallT(SVar("w_27"), [], [])
                            , Seq(
                                Match(Var("v_49"))
                              , Build(
                                  Anno(
                                    Op(
                                      "SignDecl"
                                    , [Var("t_49"), Var("u_49"), Var("v_49")]
                                    )
                                  , Var("s_49")
                                  )
                                )
                              )
                            )
                          )
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
            ["z_49", "w_49", "x_49", "y_49", "a_50", "b_50", "c_50"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "ClassMulti"
                  , [Var("w_49"), Var("x_49"), Var("y_49")]
                  )
                , Var("z_49")
                )
              )
            , Seq(
                Build(Var("w_49"))
              , Seq(
                  CallT(SVar("x_27"), [], [])
                , Seq(
                    Match(Var("a_50"))
                  , Seq(
                      Build(Var("x_49"))
                    , Seq(
                        CallT(SVar("y_27"), [], [])
                      , Seq(
                          Match(Var("b_50"))
                        , Seq(
                            Build(Var("y_49"))
                          , Seq(
                              CallT(SVar("z_27"), [], [])
                            , Seq(
                                Match(Var("c_50"))
                              , Build(
                                  Anno(
                                    Op(
                                      "ClassMulti"
                                    , [Var("a_50"), Var("b_50"), Var("c_50")]
                                    )
                                  , Var("z_49")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["f_50", "d_50", "e_50", "g_50", "h_50"]
          , Seq(
              Match(
                Anno(
                  Op("SimpleClass", [Var("d_50"), Var("e_50")])
                , Var("f_50")
                )
              )
            , Seq(
                Build(Var("d_50"))
              , Seq(
                  CallT(SVar("a_28"), [], [])
                , Seq(
                    Match(Var("g_50"))
                  , Seq(
                      Build(Var("e_50"))
                    , Seq(
                        CallT(SVar("b_28"), [], [])
                      , Seq(
                          Match(Var("h_50"))
                        , Build(
                            Anno(
                              Op("SimpleClass", [Var("g_50"), Var("h_50")])
                            , Var("f_50")
                            )
                          )
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
              "c_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_50", "i_50", "k_50"]
          , Seq(
              Match(
                Anno(Op("SContext", [Var("i_50")]), Var("j_50"))
              )
            , Seq(
                Build(Var("i_50"))
              , Seq(
                  CallT(SVar("c_28"), [], [])
                , Seq(
                    Match(Var("k_50"))
                  , Build(
                      Anno(Op("SContext", [Var("k_50")]), Var("j_50"))
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
              "d_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_50", "l_50", "n_50"]
          , Seq(
              Match(
                Anno(Op("Context", [Var("l_50")]), Var("m_50"))
              )
            , Seq(
                Build(Var("l_50"))
              , Seq(
                  CallT(SVar("d_28"), [], [])
                , Seq(
                    Match(Var("n_50"))
                  , Build(
                      Anno(Op("Context", [Var("n_50")]), Var("m_50"))
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
              "e_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "f_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_50", "o_50", "p_50", "r_50", "s_50"]
          , Seq(
              Match(
                Anno(
                  Op("InstArrow", [Var("o_50"), Var("p_50")])
                , Var("q_50")
                )
              )
            , Seq(
                Build(Var("o_50"))
              , Seq(
                  CallT(SVar("e_28"), [], [])
                , Seq(
                    Match(Var("r_50"))
                  , Seq(
                      Build(Var("p_50"))
                    , Seq(
                        CallT(SVar("f_28"), [], [])
                      , Seq(
                          Match(Var("s_50"))
                        , Build(
                            Anno(
                              Op("InstArrow", [Var("r_50"), Var("s_50")])
                            , Var("q_50")
                            )
                          )
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
              "g_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_50", "t_50", "v_50"]
          , Seq(
              Match(
                Anno(Op("InstList", [Var("t_50")]), Var("u_50"))
              )
            , Seq(
                Build(Var("t_50"))
              , Seq(
                  CallT(SVar("g_28"), [], [])
                , Seq(
                    Match(Var("v_50"))
                  , Build(
                      Anno(Op("InstList", [Var("v_50")]), Var("u_50"))
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
            ["y_50", "w_50", "x_50", "z_50", "a_51"]
          , Seq(
              Match(
                Anno(
                  Op("InstTuple", [Var("w_50"), Var("x_50")])
                , Var("y_50")
                )
              )
            , Seq(
                Build(Var("w_50"))
              , Seq(
                  CallT(SVar("h_28"), [], [])
                , Seq(
                    Match(Var("z_50"))
                  , Seq(
                      Build(Var("x_50"))
                    , Seq(
                        CallT(SVar("i_28"), [], [])
                      , Seq(
                          Match(Var("a_51"))
                        , Build(
                            Anno(
                              Op("InstTuple", [Var("z_50"), Var("a_51")])
                            , Var("y_50")
                            )
                          )
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
              "j_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "k_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_51", "b_51", "c_51", "e_51", "f_51"]
          , Seq(
              Match(
                Anno(
                  Op("InstApp", [Var("b_51"), Var("c_51")])
                , Var("d_51")
                )
              )
            , Seq(
                Build(Var("b_51"))
              , Seq(
                  CallT(SVar("j_28"), [], [])
                , Seq(
                    Match(Var("e_51"))
                  , Seq(
                      Build(Var("c_51"))
                    , Seq(
                        CallT(SVar("k_28"), [], [])
                      , Seq(
                          Match(Var("f_51"))
                        , Build(
                            Anno(
                              Op("InstApp", [Var("e_51"), Var("f_51")])
                            , Var("d_51")
                            )
                          )
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
              "l_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_51", "g_51", "i_51"]
          , Seq(
              Match(
                Anno(Op("InstCons", [Var("g_51")]), Var("h_51"))
              )
            , Seq(
                Build(Var("g_51"))
              , Seq(
                  CallT(SVar("l_28"), [], [])
                , Seq(
                    Match(Var("i_51"))
                  , Build(
                      Anno(Op("InstCons", [Var("i_51")]), Var("h_51"))
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
          , VarDec(
              "o_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_51", "j_51", "k_51", "l_51", "n_51", "o_51", "p_51"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "InfixConstr"
                  , [Var("j_51"), Var("k_51"), Var("l_51")]
                  )
                , Var("m_51")
                )
              )
            , Seq(
                Build(Var("j_51"))
              , Seq(
                  CallT(SVar("m_28"), [], [])
                , Seq(
                    Match(Var("n_51"))
                  , Seq(
                      Build(Var("k_51"))
                    , Seq(
                        CallT(SVar("n_28"), [], [])
                      , Seq(
                          Match(Var("o_51"))
                        , Seq(
                            Build(Var("l_51"))
                          , Seq(
                              CallT(SVar("o_28"), [], [])
                            , Seq(
                                Match(Var("p_51"))
                              , Build(
                                  Anno(
                                    Op(
                                      "InfixConstr"
                                    , [Var("n_51"), Var("o_51"), Var("p_51")]
                                    )
                                  , Var("m_51")
                                  )
                                )
                              )
                            )
                          )
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
          ]
        , []
        , Scope(
            ["s_51", "q_51", "r_51", "t_51", "u_51"]
          , Seq(
              Match(
                Anno(
                  Op("ConstrDecl", [Var("q_51"), Var("r_51")])
                , Var("s_51")
                )
              )
            , Seq(
                Build(Var("q_51"))
              , Seq(
                  CallT(SVar("p_28"), [], [])
                , Seq(
                    Match(Var("t_51"))
                  , Seq(
                      Build(Var("r_51"))
                    , Seq(
                        CallT(SVar("q_28"), [], [])
                      , Seq(
                          Match(Var("u_51"))
                        , Build(
                            Anno(
                              Op("ConstrDecl", [Var("t_51"), Var("u_51")])
                            , Var("s_51")
                            )
                          )
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
              "r_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_51", "v_51", "x_51"]
          , Seq(
              Match(
                Anno(Op("ConstrDecls", [Var("v_51")]), Var("w_51"))
              )
            , Seq(
                Build(Var("v_51"))
              , Seq(
                  CallT(SVar("r_28"), [], [])
                , Seq(
                    Match(Var("x_51"))
                  , Build(
                      Anno(Op("ConstrDecls", [Var("x_51")]), Var("w_51"))
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
              "s_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_51", "y_51", "a_52"]
          , Seq(
              Match(
                Anno(Op("Derive", [Var("y_51")]), Var("z_51"))
              )
            , Seq(
                Build(Var("y_51"))
              , Seq(
                  CallT(SVar("s_28"), [], [])
                , Seq(
                    Match(Var("a_52"))
                  , Build(
                      Anno(Op("Derive", [Var("a_52")]), Var("z_51"))
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
              "t_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_52", "b_52", "c_52", "e_52", "f_52"]
          , Seq(
              Match(
                Anno(
                  Op("TFunBin", [Var("b_52"), Var("c_52")])
                , Var("d_52")
                )
              )
            , Seq(
                Build(Var("b_52"))
              , Seq(
                  CallT(SVar("t_28"), [], [])
                , Seq(
                    Match(Var("e_52"))
                  , Seq(
                      Build(Var("c_52"))
                    , Seq(
                        CallT(SVar("u_28"), [], [])
                      , Seq(
                          Match(Var("f_52"))
                        , Build(
                            Anno(
                              Op("TFunBin", [Var("e_52"), Var("f_52")])
                            , Var("d_52")
                            )
                          )
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
              "v_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "w_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_52", "g_52", "h_52", "j_52", "k_52"]
          , Seq(
              Match(
                Anno(
                  Op("TAppBin", [Var("g_52"), Var("h_52")])
                , Var("i_52")
                )
              )
            , Seq(
                Build(Var("g_52"))
              , Seq(
                  CallT(SVar("v_28"), [], [])
                , Seq(
                    Match(Var("j_52"))
                  , Seq(
                      Build(Var("h_52"))
                    , Seq(
                        CallT(SVar("w_28"), [], [])
                      , Seq(
                          Match(Var("k_52"))
                        , Build(
                            Anno(
                              Op("TAppBin", [Var("j_52"), Var("k_52")])
                            , Var("i_52")
                            )
                          )
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
              "x_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_52", "l_52", "n_52"]
          , Seq(
              Match(
                Anno(Op("TProd", [Var("l_52")]), Var("m_52"))
              )
            , Seq(
                Build(Var("l_52"))
              , Seq(
                  CallT(SVar("x_28"), [], [])
                , Seq(
                    Match(Var("n_52"))
                  , Build(
                      Anno(Op("TProd", [Var("n_52")]), Var("m_52"))
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
              "y_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_52", "o_52", "q_52"]
          , Seq(
              Match(
                Anno(Op("TList", [Var("o_52")]), Var("p_52"))
              )
            , Seq(
                Build(Var("o_52"))
              , Seq(
                  CallT(SVar("y_28"), [], [])
                , Seq(
                    Match(Var("q_52"))
                  , Build(
                      Anno(Op("TList", [Var("q_52")]), Var("p_52"))
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
              "z_28"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["s_52", "r_52", "t_52"]
          , Seq(
              Match(
                Anno(Op("TVar", [Var("r_52")]), Var("s_52"))
              )
            , Seq(
                Build(Var("r_52"))
              , Seq(
                  CallT(SVar("z_28"), [], [])
                , Seq(
                    Match(Var("t_52"))
                  , Build(
                      Anno(Op("TVar", [Var("t_52")]), Var("s_52"))
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
              "a_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_52", "u_52", "w_52"]
          , Seq(
              Match(
                Anno(Op("TCon", [Var("u_52")]), Var("v_52"))
              )
            , Seq(
                Build(Var("u_52"))
              , Seq(
                  CallT(SVar("a_29"), [], [])
                , Seq(
                    Match(Var("w_52"))
                  , Build(
                      Anno(Op("TCon", [Var("w_52")]), Var("v_52"))
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
              "b_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "c_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_52", "x_52", "y_52", "a_53", "b_53"]
          , Seq(
              Match(
                Anno(
                  Op("TCons", [Var("x_52"), Var("y_52")])
                , Var("z_52")
                )
              )
            , Seq(
                Build(Var("x_52"))
              , Seq(
                  CallT(SVar("b_29"), [], [])
                , Seq(
                    Match(Var("a_53"))
                  , Seq(
                      Build(Var("y_52"))
                    , Seq(
                        CallT(SVar("c_29"), [], [])
                      , Seq(
                          Match(Var("b_53"))
                        , Build(
                            Anno(
                              Op("TCons", [Var("a_53"), Var("b_53")])
                            , Var("z_52")
                            )
                          )
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
              "d_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_53", "c_53", "e_53"]
          , Seq(
              Match(
                Anno(Op("Hiding", [Var("c_53")]), Var("d_53"))
              )
            , Seq(
                Build(Var("c_53"))
              , Seq(
                  CallT(SVar("d_29"), [], [])
                , Seq(
                    Match(Var("e_53"))
                  , Build(
                      Anno(Op("Hiding", [Var("e_53")]), Var("d_53"))
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
              "e_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_53", "f_53", "h_53"]
          , Seq(
              Match(
                Anno(Op("Impspec", [Var("f_53")]), Var("g_53"))
              )
            , Seq(
                Build(Var("f_53"))
              , Seq(
                  CallT(SVar("e_29"), [], [])
                , Seq(
                    Match(Var("h_53"))
                  , Build(
                      Anno(Op("Impspec", [Var("h_53")]), Var("g_53"))
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
              "f_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_53", "i_53", "k_53"]
          , Seq(
              Match(
                Anno(Op("As", [Var("i_53")]), Var("j_53"))
              )
            , Seq(
                Build(Var("i_53"))
              , Seq(
                  CallT(SVar("f_29"), [], [])
                , Seq(
                    Match(Var("k_53"))
                  , Build(
                      Anno(Op("As", [Var("k_53")]), Var("j_53"))
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
              "g_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "h_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "i_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
          ]
        , []
        , Scope(
            [ "q_53"
            , "l_53"
            , "m_53"
            , "n_53"
            , "o_53"
            , "p_53"
            , "r_53"
            , "s_53"
            , "t_53"
            , "u_53"
            , "v_53"
            ]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Import"
                  , [ Var("l_53")
                    , Var("m_53")
                    , Var("n_53")
                    , Var("o_53")
                    , Var("p_53")
                    ]
                  )
                , Var("q_53")
                )
              )
            , Seq(
                Build(Var("l_53"))
              , Seq(
                  CallT(SVar("g_29"), [], [])
                , Seq(
                    Match(Var("r_53"))
                  , Seq(
                      Build(Var("m_53"))
                    , Seq(
                        CallT(SVar("h_29"), [], [])
                      , Seq(
                          Match(Var("s_53"))
                        , Seq(
                            Build(Var("n_53"))
                          , Seq(
                              CallT(SVar("i_29"), [], [])
                            , Seq(
                                Match(Var("t_53"))
                              , Seq(
                                  Build(Var("o_53"))
                                , Seq(
                                    CallT(SVar("j_29"), [], [])
                                  , Seq(
                                      Match(Var("u_53"))
                                    , Seq(
                                        Build(Var("p_53"))
                                      , Seq(
                                          CallT(SVar("k_29"), [], [])
                                        , Seq(
                                            Match(Var("v_53"))
                                          , Build(
                                              Anno(
                                                Op(
                                                  "Import"
                                                , [ Var("r_53")
                                                  , Var("s_53")
                                                  , Var("t_53")
                                                  , Var("u_53")
                                                  , Var("v_53")
                                                  ]
                                                )
                                              , Var("q_53")
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
              "l_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_53", "w_53", "y_53"]
          , Seq(
              Match(
                Anno(Op("Exports", [Var("w_53")]), Var("x_53"))
              )
            , Seq(
                Build(Var("w_53"))
              , Seq(
                  CallT(SVar("l_29"), [], [])
                , Seq(
                    Match(Var("y_53"))
                  , Build(
                      Anno(Op("Exports", [Var("y_53")]), Var("x_53"))
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
              "m_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_54", "z_53", "b_54"]
          , Seq(
              Match(
                Anno(Op("Exportlist", [Var("z_53")]), Var("a_54"))
              )
            , Seq(
                Build(Var("z_53"))
              , Seq(
                  CallT(SVar("m_29"), [], [])
                , Seq(
                    Match(Var("b_54"))
                  , Build(
                      Anno(Op("Exportlist", [Var("b_54")]), Var("a_54"))
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
              "n_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "o_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_54", "c_54", "d_54", "f_54", "g_54"]
          , Seq(
              Match(
                Anno(
                  Op("TopdeclSeq", [Var("c_54"), Var("d_54")])
                , Var("e_54")
                )
              )
            , Seq(
                Build(Var("c_54"))
              , Seq(
                  CallT(SVar("n_29"), [], [])
                , Seq(
                    Match(Var("f_54"))
                  , Seq(
                      Build(Var("d_54"))
                    , Seq(
                        CallT(SVar("o_29"), [], [])
                      , Seq(
                          Match(Var("g_54"))
                        , Build(
                            Anno(
                              Op("TopdeclSeq", [Var("f_54"), Var("g_54")])
                            , Var("e_54")
                            )
                          )
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
              "p_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "q_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_54", "h_54", "i_54", "k_54", "l_54"]
          , Seq(
              Match(
                Anno(
                  Op("ImportdeclSeq", [Var("h_54"), Var("i_54")])
                , Var("j_54")
                )
              )
            , Seq(
                Build(Var("h_54"))
              , Seq(
                  CallT(SVar("p_29"), [], [])
                , Seq(
                    Match(Var("k_54"))
                  , Seq(
                      Build(Var("i_54"))
                    , Seq(
                        CallT(SVar("q_29"), [], [])
                      , Seq(
                          Match(Var("l_54"))
                        , Build(
                            Anno(
                              Op("ImportdeclSeq", [Var("k_54"), Var("l_54")])
                            , Var("j_54")
                            )
                          )
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
              "r_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "s_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_54", "m_54", "n_54", "p_54", "q_54"]
          , Seq(
              Match(
                Anno(
                  Op("OffBody", [Var("m_54"), Var("n_54")])
                , Var("o_54")
                )
              )
            , Seq(
                Build(Var("m_54"))
              , Seq(
                  CallT(SVar("r_29"), [], [])
                , Seq(
                    Match(Var("p_54"))
                  , Seq(
                      Build(Var("n_54"))
                    , Seq(
                        CallT(SVar("s_29"), [], [])
                      , Seq(
                          Match(Var("q_54"))
                        , Build(
                            Anno(
                              Op("OffBody", [Var("p_54"), Var("q_54")])
                            , Var("o_54")
                            )
                          )
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
              "t_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_54", "r_54", "s_54", "u_54", "v_54"]
          , Seq(
              Match(
                Anno(
                  Op("Body", [Var("r_54"), Var("s_54")])
                , Var("t_54")
                )
              )
            , Seq(
                Build(Var("r_54"))
              , Seq(
                  CallT(SVar("t_29"), [], [])
                , Seq(
                    Match(Var("u_54"))
                  , Seq(
                      Build(Var("s_54"))
                    , Seq(
                        CallT(SVar("u_29"), [], [])
                      , Seq(
                          Match(Var("v_54"))
                        , Build(
                            Anno(
                              Op("Body", [Var("u_54"), Var("v_54")])
                            , Var("t_54")
                            )
                          )
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
              "v_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
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
          , VarDec(
              "y_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_55", "w_54", "x_54", "y_54", "z_54", "b_55", "c_55", "d_55", "e_55"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "FlexibleInstance"
                  , [Var("w_54"), Var("x_54"), Var("y_54"), Var("z_54")]
                  )
                , Var("a_55")
                )
              )
            , Seq(
                Build(Var("w_54"))
              , Seq(
                  CallT(SVar("v_29"), [], [])
                , Seq(
                    Match(Var("b_55"))
                  , Seq(
                      Build(Var("x_54"))
                    , Seq(
                        CallT(SVar("w_29"), [], [])
                      , Seq(
                          Match(Var("c_55"))
                        , Seq(
                            Build(Var("y_54"))
                          , Seq(
                              CallT(SVar("x_29"), [], [])
                            , Seq(
                                Match(Var("d_55"))
                              , Seq(
                                  Build(Var("z_54"))
                                , Seq(
                                    CallT(SVar("y_29"), [], [])
                                  , Seq(
                                      Match(Var("e_55"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "FlexibleInstance"
                                          , [Var("b_55"), Var("c_55"), Var("d_55"), Var("e_55")]
                                          )
                                        , Var("a_55")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
              "z_29"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_55", "f_55", "h_55"]
          , Seq(
              Match(
                Anno(Op("Default", [Var("f_55")]), Var("g_55"))
              )
            , Seq(
                Build(Var("f_55"))
              , Seq(
                  CallT(SVar("z_29"), [], [])
                , Seq(
                    Match(Var("h_55"))
                  , Build(
                      Anno(Op("Default", [Var("h_55")]), Var("g_55"))
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
          , VarDec(
              "c_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_55", "i_55", "j_55", "k_55", "l_55", "n_55", "o_55", "p_55", "q_55"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Instance"
                  , [Var("i_55"), Var("j_55"), Var("k_55"), Var("l_55")]
                  )
                , Var("m_55")
                )
              )
            , Seq(
                Build(Var("i_55"))
              , Seq(
                  CallT(SVar("a_30"), [], [])
                , Seq(
                    Match(Var("n_55"))
                  , Seq(
                      Build(Var("j_55"))
                    , Seq(
                        CallT(SVar("b_30"), [], [])
                      , Seq(
                          Match(Var("o_55"))
                        , Seq(
                            Build(Var("k_55"))
                          , Seq(
                              CallT(SVar("c_30"), [], [])
                            , Seq(
                                Match(Var("p_55"))
                              , Seq(
                                  Build(Var("l_55"))
                                , Seq(
                                    CallT(SVar("d_30"), [], [])
                                  , Seq(
                                      Match(Var("q_55"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "Instance"
                                          , [Var("n_55"), Var("o_55"), Var("p_55"), Var("q_55")]
                                          )
                                        , Var("m_55")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
          , VarDec(
              "h_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["v_55", "r_55", "s_55", "t_55", "u_55", "w_55", "x_55", "y_55", "z_55"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Class"
                  , [Var("r_55"), Var("s_55"), Var("t_55"), Var("u_55")]
                  )
                , Var("v_55")
                )
              )
            , Seq(
                Build(Var("r_55"))
              , Seq(
                  CallT(SVar("e_30"), [], [])
                , Seq(
                    Match(Var("w_55"))
                  , Seq(
                      Build(Var("s_55"))
                    , Seq(
                        CallT(SVar("f_30"), [], [])
                      , Seq(
                          Match(Var("x_55"))
                        , Seq(
                            Build(Var("t_55"))
                          , Seq(
                              CallT(SVar("g_30"), [], [])
                            , Seq(
                                Match(Var("y_55"))
                              , Seq(
                                  Build(Var("u_55"))
                                , Seq(
                                    CallT(SVar("h_30"), [], [])
                                  , Seq(
                                      Match(Var("z_55"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "Class"
                                          , [Var("w_55"), Var("x_55"), Var("y_55"), Var("z_55")]
                                          )
                                        , Var("v_55")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
          , VarDec(
              "l_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_56", "a_56", "b_56", "c_56", "d_56", "f_56", "g_56", "h_56", "i_56"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Data"
                  , [Var("a_56"), Var("b_56"), Var("c_56"), Var("d_56")]
                  )
                , Var("e_56")
                )
              )
            , Seq(
                Build(Var("a_56"))
              , Seq(
                  CallT(SVar("i_30"), [], [])
                , Seq(
                    Match(Var("f_56"))
                  , Seq(
                      Build(Var("b_56"))
                    , Seq(
                        CallT(SVar("j_30"), [], [])
                      , Seq(
                          Match(Var("g_56"))
                        , Seq(
                            Build(Var("c_56"))
                          , Seq(
                              CallT(SVar("k_30"), [], [])
                            , Seq(
                                Match(Var("h_56"))
                              , Seq(
                                  Build(Var("d_56"))
                                , Seq(
                                    CallT(SVar("l_30"), [], [])
                                  , Seq(
                                      Match(Var("i_56"))
                                    , Build(
                                        Anno(
                                          Op(
                                            "Data"
                                          , [Var("f_56"), Var("g_56"), Var("h_56"), Var("i_56")]
                                          )
                                        , Var("e_56")
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
            ["m_56", "j_56", "k_56", "l_56", "n_56", "o_56", "p_56"]
          , Seq(
              Match(
                Anno(
                  Op(
                    "TypeDecl"
                  , [Var("j_56"), Var("k_56"), Var("l_56")]
                  )
                , Var("m_56")
                )
              )
            , Seq(
                Build(Var("j_56"))
              , Seq(
                  CallT(SVar("m_30"), [], [])
                , Seq(
                    Match(Var("n_56"))
                  , Seq(
                      Build(Var("k_56"))
                    , Seq(
                        CallT(SVar("n_30"), [], [])
                      , Seq(
                          Match(Var("o_56"))
                        , Seq(
                            Build(Var("l_56"))
                          , Seq(
                              CallT(SVar("o_30"), [], [])
                            , Seq(
                                Match(Var("p_56"))
                              , Build(
                                  Anno(
                                    Op(
                                      "TypeDecl"
                                    , [Var("n_56"), Var("o_56"), Var("p_56")]
                                    )
                                  , Var("m_56")
                                  )
                                )
                              )
                            )
                          )
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
              "p_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_56", "q_56", "s_56"]
          , Seq(
              Match(
                Anno(Op("Program", [Var("q_56")]), Var("r_56"))
              )
            , Seq(
                Build(Var("q_56"))
              , Seq(
                  CallT(SVar("p_30"), [], [])
                , Seq(
                    Match(Var("s_56"))
                  , Build(
                      Anno(Op("Program", [Var("s_56")]), Var("r_56"))
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
            ["v_56", "t_56", "u_56", "w_56", "x_56"]
          , Seq(
              Match(
                Anno(
                  Op("Module", [Var("t_56"), Var("u_56")])
                , Var("v_56")
                )
              )
            , Seq(
                Build(Var("t_56"))
              , Seq(
                  CallT(SVar("q_30"), [], [])
                , Seq(
                    Match(Var("w_56"))
                  , Seq(
                      Build(Var("u_56"))
                    , Seq(
                        CallT(SVar("r_30"), [], [])
                      , Seq(
                          Match(Var("x_56"))
                        , Build(
                            Anno(
                              Op("Module", [Var("w_56"), Var("x_56")])
                            , Var("v_56")
                            )
                          )
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
              "s_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_57", "y_56", "z_56", "b_57", "c_57"]
          , Seq(
              Match(
                Anno(
                  Op("ModuleDec", [Var("y_56"), Var("z_56")])
                , Var("a_57")
                )
              )
            , Seq(
                Build(Var("y_56"))
              , Seq(
                  CallT(SVar("s_30"), [], [])
                , Seq(
                    Match(Var("b_57"))
                  , Seq(
                      Build(Var("z_56"))
                    , Seq(
                        CallT(SVar("t_30"), [], [])
                      , Seq(
                          Match(Var("c_57"))
                        , Build(
                            Anno(
                              Op("ModuleDec", [Var("b_57"), Var("c_57")])
                            , Var("a_57")
                            )
                          )
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
              "u_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_57", "d_57", "f_57"]
          , Seq(
              Match(
                Anno(Op("CLitLit", [Var("d_57")]), Var("e_57"))
              )
            , Seq(
                Build(Var("d_57"))
              , Seq(
                  CallT(SVar("u_30"), [], [])
                , Seq(
                    Match(Var("f_57"))
                  , Build(
                      Anno(Op("CLitLit", [Var("f_57")]), Var("e_57"))
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
              "v_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_57", "g_57", "i_57"]
          , Seq(
              Match(
                Anno(Op("PrimDouble", [Var("g_57")]), Var("h_57"))
              )
            , Seq(
                Build(Var("g_57"))
              , Seq(
                  CallT(SVar("v_30"), [], [])
                , Seq(
                    Match(Var("i_57"))
                  , Build(
                      Anno(Op("PrimDouble", [Var("i_57")]), Var("h_57"))
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
              "w_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_57", "j_57", "l_57"]
          , Seq(
              Match(
                Anno(Op("PrimFloat", [Var("j_57")]), Var("k_57"))
              )
            , Seq(
                Build(Var("j_57"))
              , Seq(
                  CallT(SVar("w_30"), [], [])
                , Seq(
                    Match(Var("l_57"))
                  , Build(
                      Anno(Op("PrimFloat", [Var("l_57")]), Var("k_57"))
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
              "x_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["n_57", "m_57", "o_57"]
          , Seq(
              Match(
                Anno(Op("PrimString", [Var("m_57")]), Var("n_57"))
              )
            , Seq(
                Build(Var("m_57"))
              , Seq(
                  CallT(SVar("x_30"), [], [])
                , Seq(
                    Match(Var("o_57"))
                  , Build(
                      Anno(Op("PrimString", [Var("o_57")]), Var("n_57"))
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
              "y_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_57", "p_57", "r_57"]
          , Seq(
              Match(
                Anno(Op("PrimChar", [Var("p_57")]), Var("q_57"))
              )
            , Seq(
                Build(Var("p_57"))
              , Seq(
                  CallT(SVar("y_30"), [], [])
                , Seq(
                    Match(Var("r_57"))
                  , Build(
                      Anno(Op("PrimChar", [Var("r_57")]), Var("q_57"))
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
              "z_30"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_57", "s_57", "u_57"]
          , Seq(
              Match(
                Anno(Op("PrimInt", [Var("s_57")]), Var("t_57"))
              )
            , Seq(
                Build(Var("s_57"))
              , Seq(
                  CallT(SVar("z_30"), [], [])
                , Seq(
                    Match(Var("u_57"))
                  , Build(
                      Anno(Op("PrimInt", [Var("u_57")]), Var("t_57"))
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
              "a_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_57", "v_57", "x_57"]
          , Seq(
              Match(
                Anno(Op("Float", [Var("v_57")]), Var("w_57"))
              )
            , Seq(
                Build(Var("v_57"))
              , Seq(
                  CallT(SVar("a_31"), [], [])
                , Seq(
                    Match(Var("x_57"))
                  , Build(
                      Anno(Op("Float", [Var("x_57")]), Var("w_57"))
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
              "b_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["z_57", "y_57", "a_58"]
          , Seq(
              Match(
                Anno(Op("Int", [Var("y_57")]), Var("z_57"))
              )
            , Seq(
                Build(Var("y_57"))
              , Seq(
                  CallT(SVar("b_31"), [], [])
                , Seq(
                    Match(Var("a_58"))
                  , Build(
                      Anno(Op("Int", [Var("a_58")]), Var("z_57"))
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
              "c_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_58", "b_58", "d_58"]
          , Seq(
              Match(
                Anno(Op("HexadecimalEsc", [Var("b_58")]), Var("c_58"))
              )
            , Seq(
                Build(Var("b_58"))
              , Seq(
                  CallT(SVar("c_31"), [], [])
                , Seq(
                    Match(Var("d_58"))
                  , Build(
                      Anno(Op("HexadecimalEsc", [Var("d_58")]), Var("c_58"))
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
              "d_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_58", "e_58", "g_58"]
          , Seq(
              Match(
                Anno(Op("OctalEsc", [Var("e_58")]), Var("f_58"))
              )
            , Seq(
                Build(Var("e_58"))
              , Seq(
                  CallT(SVar("d_31"), [], [])
                , Seq(
                    Match(Var("g_58"))
                  , Build(
                      Anno(Op("OctalEsc", [Var("g_58")]), Var("f_58"))
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
              "e_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_58", "h_58", "j_58"]
          , Seq(
              Match(
                Anno(Op("DecimalEsc", [Var("h_58")]), Var("i_58"))
              )
            , Seq(
                Build(Var("h_58"))
              , Seq(
                  CallT(SVar("e_31"), [], [])
                , Seq(
                    Match(Var("j_58"))
                  , Build(
                      Anno(Op("DecimalEsc", [Var("j_58")]), Var("i_58"))
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
              "f_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_58", "k_58", "m_58"]
          , Seq(
              Match(
                Anno(Op("ASCIIEsc", [Var("k_58")]), Var("l_58"))
              )
            , Seq(
                Build(Var("k_58"))
              , Seq(
                  CallT(SVar("f_31"), [], [])
                , Seq(
                    Match(Var("m_58"))
                  , Build(
                      Anno(Op("ASCIIEsc", [Var("m_58")]), Var("l_58"))
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
              "g_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_58", "n_58", "p_58"]
          , Seq(
              Match(
                Anno(Op("CharEsc", [Var("n_58")]), Var("o_58"))
              )
            , Seq(
                Build(Var("n_58"))
              , Seq(
                  CallT(SVar("g_31"), [], [])
                , Seq(
                    Match(Var("p_58"))
                  , Build(
                      Anno(Op("CharEsc", [Var("p_58")]), Var("o_58"))
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
              "h_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_58", "q_58", "s_58"]
          , Seq(
              Match(
                Anno(Op("Gap", [Var("q_58")]), Var("r_58"))
              )
            , Seq(
                Build(Var("q_58"))
              , Seq(
                  CallT(SVar("h_31"), [], [])
                , Seq(
                    Match(Var("s_58"))
                  , Build(
                      Anno(Op("Gap", [Var("s_58")]), Var("r_58"))
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
              "i_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_58", "t_58", "v_58"]
          , Seq(
              Match(
                Anno(Op("EscapeString", [Var("t_58")]), Var("u_58"))
              )
            , Seq(
                Build(Var("t_58"))
              , Seq(
                  CallT(SVar("i_31"), [], [])
                , Seq(
                    Match(Var("v_58"))
                  , Build(
                      Anno(Op("EscapeString", [Var("v_58")]), Var("u_58"))
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
              "j_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_58", "w_58", "y_58"]
          , Seq(
              Match(
                Anno(Op("Escape", [Var("w_58")]), Var("x_58"))
              )
            , Seq(
                Build(Var("w_58"))
              , Seq(
                  CallT(SVar("j_31"), [], [])
                , Seq(
                    Match(Var("y_58"))
                  , Build(
                      Anno(Op("Escape", [Var("y_58")]), Var("x_58"))
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
              "k_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_59", "z_58", "b_59"]
          , Seq(
              Match(
                Anno(Op("String", [Var("z_58")]), Var("a_59"))
              )
            , Seq(
                Build(Var("z_58"))
              , Seq(
                  CallT(SVar("k_31"), [], [])
                , Seq(
                    Match(Var("b_59"))
                  , Build(
                      Anno(Op("String", [Var("b_59")]), Var("a_59"))
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
              "l_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_59", "c_59", "e_59"]
          , Seq(
              Match(
                Anno(Op("Char", [Var("c_59")]), Var("d_59"))
              )
            , Seq(
                Build(Var("c_59"))
              , Seq(
                  CallT(SVar("l_31"), [], [])
                , Seq(
                    Match(Var("e_59"))
                  , Build(
                      Anno(Op("Char", [Var("e_59")]), Var("d_59"))
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
              "m_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "n_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["h_59", "f_59", "g_59", "i_59", "j_59"]
          , Seq(
              Match(
                Anno(
                  Op("QModId", [Var("f_59"), Var("g_59")])
                , Var("h_59")
                )
              )
            , Seq(
                Build(Var("f_59"))
              , Seq(
                  CallT(SVar("m_31"), [], [])
                , Seq(
                    Match(Var("i_59"))
                  , Seq(
                      Build(Var("g_59"))
                    , Seq(
                        CallT(SVar("n_31"), [], [])
                      , Seq(
                          Match(Var("j_59"))
                        , Build(
                            Anno(
                              Op("QModId", [Var("i_59"), Var("j_59")])
                            , Var("h_59")
                            )
                          )
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
              "o_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "p_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_59", "k_59", "l_59", "n_59", "o_59"]
          , Seq(
              Match(
                Anno(
                  Op("QConSym", [Var("k_59"), Var("l_59")])
                , Var("m_59")
                )
              )
            , Seq(
                Build(Var("k_59"))
              , Seq(
                  CallT(SVar("o_31"), [], [])
                , Seq(
                    Match(Var("n_59"))
                  , Seq(
                      Build(Var("l_59"))
                    , Seq(
                        CallT(SVar("p_31"), [], [])
                      , Seq(
                          Match(Var("o_59"))
                        , Build(
                            Anno(
                              Op("QConSym", [Var("n_59"), Var("o_59")])
                            , Var("m_59")
                            )
                          )
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
              "q_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "r_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_59", "p_59", "q_59", "s_59", "t_59"]
          , Seq(
              Match(
                Anno(
                  Op("QVarSym", [Var("p_59"), Var("q_59")])
                , Var("r_59")
                )
              )
            , Seq(
                Build(Var("p_59"))
              , Seq(
                  CallT(SVar("q_31"), [], [])
                , Seq(
                    Match(Var("s_59"))
                  , Seq(
                      Build(Var("q_59"))
                    , Seq(
                        CallT(SVar("r_31"), [], [])
                      , Seq(
                          Match(Var("t_59"))
                        , Build(
                            Anno(
                              Op("QVarSym", [Var("s_59"), Var("t_59")])
                            , Var("r_59")
                            )
                          )
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
              "s_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "t_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["w_59", "u_59", "v_59", "x_59", "y_59"]
          , Seq(
              Match(
                Anno(
                  Op("QConId", [Var("u_59"), Var("v_59")])
                , Var("w_59")
                )
              )
            , Seq(
                Build(Var("u_59"))
              , Seq(
                  CallT(SVar("s_31"), [], [])
                , Seq(
                    Match(Var("x_59"))
                  , Seq(
                      Build(Var("v_59"))
                    , Seq(
                        CallT(SVar("t_31"), [], [])
                      , Seq(
                          Match(Var("y_59"))
                        , Build(
                            Anno(
                              Op("QConId", [Var("x_59"), Var("y_59")])
                            , Var("w_59")
                            )
                          )
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
              "u_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "v_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["b_60", "z_59", "a_60", "c_60", "d_60"]
          , Seq(
              Match(
                Anno(
                  Op("QVarId", [Var("z_59"), Var("a_60")])
                , Var("b_60")
                )
              )
            , Seq(
                Build(Var("z_59"))
              , Seq(
                  CallT(SVar("u_31"), [], [])
                , Seq(
                    Match(Var("c_60"))
                  , Seq(
                      Build(Var("a_60"))
                    , Seq(
                        CallT(SVar("v_31"), [], [])
                      , Seq(
                          Match(Var("d_60"))
                        , Build(
                            Anno(
                              Op("QVarId", [Var("c_60"), Var("d_60")])
                            , Var("b_60")
                            )
                          )
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
              "w_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["f_60", "e_60", "g_60"]
          , Seq(
              Match(
                Anno(Op("BinCon", [Var("e_60")]), Var("f_60"))
              )
            , Seq(
                Build(Var("e_60"))
              , Seq(
                  CallT(SVar("w_31"), [], [])
                , Seq(
                    Match(Var("g_60"))
                  , Build(
                      Anno(Op("BinCon", [Var("g_60")]), Var("f_60"))
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
              "x_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_60", "h_60", "j_60"]
          , Seq(
              Match(
                Anno(Op("ConsOp", [Var("h_60")]), Var("i_60"))
              )
            , Seq(
                Build(Var("h_60"))
              , Seq(
                  CallT(SVar("x_31"), [], [])
                , Seq(
                    Match(Var("j_60"))
                  , Build(
                      Anno(Op("ConsOp", [Var("j_60")]), Var("i_60"))
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
              "y_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["l_60", "k_60", "m_60"]
          , Seq(
              Match(
                Anno(Op("QPrefCon", [Var("k_60")]), Var("l_60"))
              )
            , Seq(
                Build(Var("k_60"))
              , Seq(
                  CallT(SVar("y_31"), [], [])
                , Seq(
                    Match(Var("m_60"))
                  , Build(
                      Anno(Op("QPrefCon", [Var("m_60")]), Var("l_60"))
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
              "z_31"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_60", "n_60", "p_60"]
          , Seq(
              Match(
                Anno(Op("PrefCon", [Var("n_60")]), Var("o_60"))
              )
            , Seq(
                Build(Var("n_60"))
              , Seq(
                  CallT(SVar("z_31"), [], [])
                , Seq(
                    Match(Var("p_60"))
                  , Build(
                      Anno(Op("PrefCon", [Var("p_60")]), Var("o_60"))
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
              "a_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_60", "q_60", "s_60"]
          , Seq(
              Match(
                Anno(Op("QPrefOp", [Var("q_60")]), Var("r_60"))
              )
            , Seq(
                Build(Var("q_60"))
              , Seq(
                  CallT(SVar("a_32"), [], [])
                , Seq(
                    Match(Var("s_60"))
                  , Build(
                      Anno(Op("QPrefOp", [Var("s_60")]), Var("r_60"))
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
              "b_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_60", "t_60", "v_60"]
          , Seq(
              Match(
                Anno(Op("PrefOp", [Var("t_60")]), Var("u_60"))
              )
            , Seq(
                Build(Var("t_60"))
              , Seq(
                  CallT(SVar("b_32"), [], [])
                , Seq(
                    Match(Var("v_60"))
                  , Build(
                      Anno(Op("PrefOp", [Var("v_60")]), Var("u_60"))
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
              "c_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["x_60", "w_60", "y_60"]
          , Seq(
              Match(
                Anno(Op("ConOp", [Var("w_60")]), Var("x_60"))
              )
            , Seq(
                Build(Var("w_60"))
              , Seq(
                  CallT(SVar("c_32"), [], [])
                , Seq(
                    Match(Var("y_60"))
                  , Build(
                      Anno(Op("ConOp", [Var("y_60")]), Var("x_60"))
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
              "d_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["a_61", "z_60", "b_61"]
          , Seq(
              Match(
                Anno(Op("Op", [Var("z_60")]), Var("a_61"))
              )
            , Seq(
                Build(Var("z_60"))
              , Seq(
                  CallT(SVar("d_32"), [], [])
                , Seq(
                    Match(Var("b_61"))
                  , Build(
                      Anno(Op("Op", [Var("b_61")]), Var("a_61"))
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
              "e_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["d_61", "c_61", "e_61"]
          , Seq(
              Match(
                Anno(Op("BinOpQ", [Var("c_61")]), Var("d_61"))
              )
            , Seq(
                Build(Var("c_61"))
              , Seq(
                  CallT(SVar("e_32"), [], [])
                , Seq(
                    Match(Var("e_61"))
                  , Build(
                      Anno(Op("BinOpQ", [Var("e_61")]), Var("d_61"))
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
              "f_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_61", "f_61", "h_61"]
          , Seq(
              Match(
                Anno(Op("BinOp", [Var("f_61")]), Var("g_61"))
              )
            , Seq(
                Build(Var("f_61"))
              , Seq(
                  CallT(SVar("f_32"), [], [])
                , Seq(
                    Match(Var("h_61"))
                  , Build(
                      Anno(Op("BinOp", [Var("h_61")]), Var("g_61"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "VarVar_1_0"
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
            ["j_61", "i_61", "k_61"]
          , Seq(
              Match(
                Anno(Op("VarVar", [Var("i_61")]), Var("j_61"))
              )
            , Seq(
                Build(Var("i_61"))
              , Seq(
                  CallT(SVar("g_32"), [], [])
                , Seq(
                    Match(Var("k_61"))
                  , Build(
                      Anno(Op("VarVar", [Var("k_61")]), Var("j_61"))
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
              "h_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_61", "l_61", "n_61"]
          , Seq(
              Match(
                Anno(Op("Ins", [Var("l_61")]), Var("m_61"))
              )
            , Seq(
                Build(Var("l_61"))
              , Seq(
                  CallT(SVar("h_32"), [], [])
                , Seq(
                    Match(Var("n_61"))
                  , Build(
                      Anno(Op("Ins", [Var("n_61")]), Var("m_61"))
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
              "i_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["q_61", "o_61", "p_61", "r_61", "s_61"]
          , Seq(
              Match(
                Anno(
                  Op("Snoc", [Var("o_61"), Var("p_61")])
                , Var("q_61")
                )
              )
            , Seq(
                Build(Var("o_61"))
              , Seq(
                  CallT(SVar("i_32"), [], [])
                , Seq(
                    Match(Var("r_61"))
                  , Seq(
                      Build(Var("p_61"))
                    , Seq(
                        CallT(SVar("j_32"), [], [])
                      , Seq(
                          Match(Var("s_61"))
                        , Build(
                            Anno(
                              Op("Snoc", [Var("r_61"), Var("s_61")])
                            , Var("q_61")
                            )
                          )
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
              "k_32"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_61", "t_61", "v_61"]
          , Seq(
              Match(
                Anno(Op("DR_UNDEFINE", [Var("t_61")]), Var("u_61"))
              )
            , Seq(
                Build(Var("t_61"))
              , Seq(
                  CallT(SVar("k_32"), [], [])
                , Seq(
                    Match(Var("v_61"))
                  , Build(
                      Anno(Op("DR_UNDEFINE", [Var("v_61")]), Var("u_61"))
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

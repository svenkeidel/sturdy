module Classes.TryCatchExample where

import Syntax

import Classes.ArrayIndexOutOfBoundsException

tryCatchExampleMainMethod :: Method
tryCatchExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (RefType "java.lang.ArrayIndexOutOfBoundsException", ["$r2", "$r3", "$r4"])
    ],
    statements = [
      Label "label1",
      Assign (VLocal "$r2") (NewExpr (RefType "java.lang.ArrayIndexOutOfBoundsException")),
      Invoke (SpecialInvoke "$r2" arrayIndexOutOfBoundsExceptionInitSignature [StringConstant "a"]),
      Throw (Local "$r2"),
      Label "label2",
      IdentityNoType "$r3" CaughtExceptionRef,
      Assign (VLocal "$r4") (NewExpr (RefType "java.lang.ArrayIndexOutOfBoundsException")),
      Invoke (SpecialInvoke "$r4" arrayIndexOutOfBoundsExceptionInitSignature [StringConstant "b"]),
      Throw (Local "$r4")
    ],
    catchClauses = [
      CatchClause { className = "java.lang.ArrayIndexOutOfBoundsException"
                  , fromLabel = "label1"
                  , toLabel   = "label2"
                  , withLabel = "label2"
                  }
    ]
  }
}

tryCatchExampleFile :: CompilationUnit
tryCatchExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "TryCatchExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    MethodMember tryCatchExampleMainMethod
  ]
}

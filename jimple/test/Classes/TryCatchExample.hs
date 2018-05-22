module Classes.TryCatchExample where

import Syntax

import Classes.ArrayIndexOutOfBoundsException

tryCatchExampleMainMethod :: Method
tryCatchExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = TVoid,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "java.lang.ArrayIndexOutOfBoundsException", ["$r2", "$r3", "$r4"])
    ],
    statements = [
      Label "label1",
      Assign (VLocal "$r2") (ENew (NewSimple (TClass "java.lang.ArrayIndexOutOfBoundsException"))),
      Invoke (SpecialInvoke "$r2" arrayIndexOutOfBoundsExceptionInitSignature [IString "a"]),
      Throw (ILocalName "$r2"),
      Label "label2",
      IdentityNoType "$r3" IDCaughtException,
      Assign (VLocal "$r4") (ENew (NewSimple (TClass "java.lang.ArrayIndexOutOfBoundsException"))),
      Invoke (SpecialInvoke "$r4" arrayIndexOutOfBoundsExceptionInitSignature [IString "b"]),
      Throw (ILocalName "$r4")
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

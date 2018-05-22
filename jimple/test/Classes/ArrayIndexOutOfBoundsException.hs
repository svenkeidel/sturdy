module Classes.ArrayIndexOutOfBoundsException where

import Syntax

import Classes.Throwable

arrayIndexOutOfBoundsExceptionInitSignature :: MethodSignature
arrayIndexOutOfBoundsExceptionInitSignature = MethodSignature
  "java.lang.ArrayIndexOutOfBoundsException"
  TVoid
  "<init>"
  [TClass "java.lang.String"]

arrayIndexOutOfBoundsExceptionInitMethod :: Method
arrayIndexOutOfBoundsExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [TClass "java.lang.String"],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "java.lang.ArrayIndexOutOfBoundsException", ["r0"]),
      (TClass "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "java.lang.ArrayIndexOutOfBoundsException"),
      Identity "s0" (ParameterRef 0) (TClass "java.lang.String"),
      Invoke (SpecialInvoke "r0" throwableInitSignature [Local "s0"]),
      Return Nothing
    ],
    catchClauses = []
  }
}

arrayIndexOutOfBoundsExceptionFile :: CompilationUnit
arrayIndexOutOfBoundsExceptionFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "java.lang.ArrayIndexOutOfBoundsException",
  extends = Just "java.lang.Throwable",
  implements = [],
  fileBody = [
    MethodMember arrayIndexOutOfBoundsExceptionInitMethod
  ]
}

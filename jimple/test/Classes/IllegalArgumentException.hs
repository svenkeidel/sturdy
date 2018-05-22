module Classes.IllegalArgumentException where

import Syntax

import Classes.Throwable

illegalArgumentExceptionInitSignature :: MethodSignature
illegalArgumentExceptionInitSignature = MethodSignature
  "java.lang.IllegalArgumentException"
  TVoid
  "<init>"
  [TClass "java.lang.String"]

illegalArgumentExceptionInitMethod :: Method
illegalArgumentExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [TClass "java.lang.String"],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "java.lang.IllegalArgumentException", ["r0"]),
      (TClass "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "java.lang.IllegalArgumentException"),
      Identity "s0" (ParameterRef 0) (TClass "java.lang.String"),
      Invoke (SpecialInvoke "r0" throwableInitSignature [Local "s0"]),
      Return Nothing
    ],
    catchClauses = []
  }
}

illegalArgumentExceptionFile :: CompilationUnit
illegalArgumentExceptionFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "java.lang.IllegalArgumentException",
  extends = Just "java.lang.Throwable",
  implements = [],
  fileBody = [
    MethodMember illegalArgumentExceptionInitMethod
  ]
}

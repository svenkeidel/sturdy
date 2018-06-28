module Java.Lang.IllegalArgumentException where

import Syntax

import Java.Lang.Throwable

illegalArgumentExceptionInitSignature :: MethodSignature
illegalArgumentExceptionInitSignature = MethodSignature
  "java.lang.IllegalArgumentException"
  VoidType
  "<init>"
  [RefType "java.lang.String"]

illegalArgumentExceptionInitMethod :: Method
illegalArgumentExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [RefType "java.lang.String"],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "java.lang.IllegalArgumentException", ["r0"]),
      (RefType "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "java.lang.IllegalArgumentException"),
      Identity "s0" (ParameterRef 0) (RefType "java.lang.String"),
      Invoke (SpecialInvoke "r0" throwableInitSignature [Local "s0"]),
      Return Nothing
    ],
    catchClauses = []
  }
}

illegalArgumentExceptionFile :: CompilationUnit
illegalArgumentExceptionFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "java.lang.IllegalArgumentException",
  extends = Just "java.lang.Throwable",
  implements = [],
  fileBody = [
    MethodMember illegalArgumentExceptionInitMethod
  ]
}

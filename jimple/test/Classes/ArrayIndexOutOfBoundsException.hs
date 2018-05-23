module Classes.ArrayIndexOutOfBoundsException where

import Syntax

import Classes.Throwable

arrayIndexOutOfBoundsExceptionInitSignature :: MethodSignature
arrayIndexOutOfBoundsExceptionInitSignature = MethodSignature
  "java.lang.ArrayIndexOutOfBoundsException"
  VoidType
  "<init>"
  [RefType "java.lang.String"]

arrayIndexOutOfBoundsExceptionInitMethod :: Method
arrayIndexOutOfBoundsExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [RefType "java.lang.String"],
  throws = [],
  methodBody = MFull {
    declarations = [
      (RefType "java.lang.ArrayIndexOutOfBoundsException", ["r0"]),
      (RefType "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "java.lang.ArrayIndexOutOfBoundsException"),
      Identity "s0" (ParameterRef 0) (RefType "java.lang.String"),
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

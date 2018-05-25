module Classes.ArithmeticException where

import Syntax

import Classes.Throwable

arithmeticExceptionInitSignature :: MethodSignature
arithmeticExceptionInitSignature = MethodSignature
  "java.lang.ArithmeticException"
  VoidType
  "<init>"
  [RefType "java.lang.String"]

arithmeticExceptionInitMethod :: Method
arithmeticExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [RefType "java.lang.String"],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "java.lang.ArithmeticException", ["r0"]),
      (RefType "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "java.lang.ArithmeticException"),
      Identity "s0" (ParameterRef 0) (RefType "java.lang.String"),
      Invoke (SpecialInvoke "r0" throwableInitSignature [Local "s0"]),
      Return Nothing
    ],
    catchClauses = []
  }
}

arithmeticExceptionFile :: CompilationUnit
arithmeticExceptionFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "java.lang.ArithmeticException",
  extends = Just "java.lang.Throwable",
  implements = [],
  fileBody = [
    MethodMember arithmeticExceptionInitMethod
  ]
}

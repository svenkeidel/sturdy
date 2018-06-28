module Java.Lang.ClassCastException where

import Syntax

import Java.Lang.Throwable

classCastExceptionInitSignature :: MethodSignature
classCastExceptionInitSignature = MethodSignature
  "java.lang.ClassCastException"
  VoidType
  "<init>"
  [RefType "java.lang.String"]

classCastExceptionInitMethod :: Method
classCastExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [RefType "java.lang.String"],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "java.lang.ClassCastException", ["r0"]),
      (RefType "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "java.lang.ClassCastException"),
      Identity "s0" (ParameterRef 0) (RefType "java.lang.String"),
      Invoke (SpecialInvoke "r0" throwableInitSignature [Local "s0"]),
      Return Nothing
    ],
    catchClauses = []
  }
}

classCastExceptionFile :: CompilationUnit
classCastExceptionFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "java.lang.ClassCastException",
  extends = Just "java.lang.Throwable",
  implements = [],
  fileBody = [
    MethodMember classCastExceptionInitMethod
  ]
}

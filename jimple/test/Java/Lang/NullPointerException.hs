module Java.Lang.NullPointerException where

import Syntax

import Java.Lang.Throwable

nullPointerExceptionInitSignature :: MethodSignature
nullPointerExceptionInitSignature = MethodSignature
  "java.lang.NullPointerException"
  VoidType
  "<init>"
  [RefType "java.lang.String"]

nullPointerExceptionInitMethod :: Method
nullPointerExceptionInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [RefType "java.lang.String"],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "java.lang.NullPointerException", ["r0"]),
      (RefType "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "java.lang.NullPointerException"),
      Identity "s0" (ParameterRef 0) (RefType "java.lang.String"),
      Invoke (SpecialInvoke "r0" throwableInitSignature [Local "s0"]),
      Return Nothing
    ],
    catchClauses = []
  }
}

nullPointerExceptionFile :: CompilationUnit
nullPointerExceptionFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "java.lang.NullPointerException",
  extends = Just "java.lang.Throwable",
  implements = [],
  fileBody = [
    MethodMember nullPointerExceptionInitMethod
  ]
}

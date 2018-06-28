module Java.Lang.Throwable where

import Syntax

import Java.Lang.Object

throwableMessageSignature :: FieldSignature
throwableMessageSignature = FieldSignature
  "java.lang.Throwable"
  (RefType "java.lang.String")
  "message"

throwableMessageField :: Field
throwableMessageField = Field {
  fieldModifiers = [Public],
  fieldType = RefType "java.lang.String",
  fieldName = "message"
}

throwableInitSignature :: MethodSignature
throwableInitSignature = MethodSignature
  "java.lang.Throwable"
  VoidType
  "<init>"
  [RefType "java.lang.String"]

throwableInitMethod :: Method
throwableInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [RefType "java.lang.String"],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "java.lang.Throwable", ["r0"]),
      (RefType "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "java.lang.Throwable"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Identity "s0" (ParameterRef 0) (RefType "java.lang.String"),
      Assign (ReferenceVar (FieldRef "r0" throwableMessageSignature)) (ImmediateExpr (Local "s0")),
      Return Nothing
    ],
    catchClauses = []
  }
}

throwableFile :: CompilationUnit
throwableFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "java.lang.Throwable",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember throwableMessageField,
    MethodMember throwableInitMethod
  ]
}

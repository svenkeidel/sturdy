module Classes.Throwable where

import Syntax

import Classes.Object

throwableMessageSignature :: FieldSignature
throwableMessageSignature = FieldSignature
  "java.lang.Throwable"
  (TClass "java.lang.String")
  "message"

throwableMessageField :: Field
throwableMessageField = Field {
  fieldModifiers = [Public],
  fieldType = TClass "java.lang.String",
  fieldName = "message"
}

throwableInitSignature :: MethodSignature
throwableInitSignature = MethodSignature
  "java.lang.Throwable"
  TVoid
  "<init>"
  [TClass "java.lang.String"]

throwableInitMethod :: Method
throwableInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [TClass "java.lang.String"],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "java.lang.Throwable", ["r0"]),
      (TClass "java.lang.String", ["s0"])
    ],
    statements = [
      Identity "r0" IDThis (TClass "java.lang.Throwable"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Identity "s0" (IDParameter 0) (TClass "java.lang.String"),
      Assign (VReference (FieldReference "r0" throwableMessageSignature)) (EImmediate (ILocalName "s0")),
      Return Nothing
    ],
    catchClauses = []
  }
}

throwableFile :: CompilationUnit
throwableFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "java.lang.Throwable",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember throwableMessageField,
    MethodMember throwableInitMethod
  ]
}

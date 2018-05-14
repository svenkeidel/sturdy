module Classes.IllegalArgumentException where

import Syntax

import Classes.Object

illegalArgumentExceptionMessageSignature :: FieldSignature
illegalArgumentExceptionMessageSignature = FieldSignature
  "java.lang.IllegalArgumentException"
  (TClass "java.lang.String")
  "message"

illegalArgumentExceptionMessageField :: Field
illegalArgumentExceptionMessageField = Field {
  fieldModifiers = [Public],
  fieldType = TClass "java.lang.String",
  fieldName = "message"
}

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
      Identity "r0" IDThis (TClass "java.lang.IllegalArgumentException"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Identity "s0" (IDParameter 0) (TClass "java.lang.String"),
      Assign (VReference (FieldReference "r0" illegalArgumentExceptionMessageSignature)) (EImmediate (ILocalName "s0")),
      Return Nothing
    ],
    catchClauses = []
  }
}

illegalArgumentExceptionFile :: File
illegalArgumentExceptionFile = File {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "java.lang.IllegalArgumentException",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember illegalArgumentExceptionMessageField,
    MethodMember illegalArgumentExceptionInitMethod
  ]
}

module Classes.PersonExample where

import Syntax

import Classes.Object

personExampleAgeSignature :: FieldSignature
personExampleAgeSignature = FieldSignature
  "PersonExample"
  TInt
  "age"

personExampleAgeField :: Field
personExampleAgeField = Field {
  fieldModifiers = [Public],
  fieldType = TInt,
  fieldName = "age"
}

personExampleMaxAgeSignature :: FieldSignature
personExampleMaxAgeSignature = FieldSignature
  "PersonExample"
  TInt
  "MAX_AGE"

personExampleMaxAgeField :: Field
personExampleMaxAgeField = Field {
  fieldModifiers = [Public, Static],
  fieldType = TInt,
  fieldName = "MAX_AGE"
}

personExampleClassInitMethod :: Method
personExampleClassInitMethod = Method {
  methodModifiers = [Public, Static],
  returnType = TVoid,
  methodName = "<clinit>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [],
    statements = [
      Assign (VReference (SignatureReference personExampleMaxAgeSignature)) (EImmediate (IInt 100))
    ],
    catchClauses = []
  }
}

personExampleMainMethod :: Method
personExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = TInt,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TInt, ["r0"]),
      (TClass "PersonExample", ["$r2"])
    ],
    statements = [
      Assign (VLocal "$r2") (ENew (NewSimple (TClass "PersonExample"))),
      Invoke (SpecialInvoke "$r2" personExampleInitSignature [IInt 10]),
      Assign (VLocal "r0") (EInvoke (VirtualInvoke "$r2" personExampleYearsLeftSignature [])),
      Return (Just (ILocalName "r0"))
    ],
    catchClauses = []
  }
}

personExampleInitSignature :: MethodSignature
personExampleInitSignature = MethodSignature
  "PersonExample"
  TVoid
  "<init>"
  [TInt]

personExampleInitMethod :: Method
personExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [TInt],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "PersonExample", ["r0"]),
      (TInt, ["i0"])
    ],
    statements = [
      Identity "r0" IDThis (TClass "PersonExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Identity "i0" (IDParameter 0) TInt,
      Assign (VReference (FieldReference "r0" personExampleAgeSignature)) (EImmediate (ILocalName "i0")),
      Return Nothing
    ],
    catchClauses = []
  }
}

personExampleYearsLeftSignature :: MethodSignature
personExampleYearsLeftSignature = MethodSignature
  "PersonExample"
  TInt
  "yearsLeft"
  [TInt]

personExampleYearsLeftMethod :: Method
personExampleYearsLeftMethod = Method {
  methodModifiers = [Private],
  returnType = TInt,
  methodName = "yearsLeft",
  parameters = [TInt],
  throws = ["java.lang.IllegalArgumentException"],
  methodBody = MFull {
    declarations = [
      (TClass "PersonExample", ["r0"]),
      (TInt, ["i0", "i1", "i2"])
    ],
    statements = [
      Identity "r0" IDThis (TClass "PersonExample"),
      Assign (VLocal "i0") (EReference (FieldReference "r0" personExampleAgeSignature)),
      Assign (VLocal "i1") (EReference (SignatureReference personExampleMaxAgeSignature)),
      Assign (VLocal "i2") (EBinop (ILocalName "i1") Minus (ILocalName "i0")),
      Return (Just (ILocalName "i2"))
    ],
    catchClauses = []
  }
}

personExampleFile :: CompilationUnit
personExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "PersonExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember personExampleMaxAgeField,
    FieldMember personExampleAgeField,
    MethodMember personExampleClassInitMethod,
    MethodMember personExampleInitMethod,
    MethodMember personExampleMainMethod,
    MethodMember personExampleYearsLeftMethod
  ]
}

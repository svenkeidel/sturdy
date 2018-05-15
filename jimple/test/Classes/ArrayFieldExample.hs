module Classes.ArrayFieldExample where

import Syntax

import Classes.Object

arrayFieldExampleArrSignature :: FieldSignature
arrayFieldExampleArrSignature = FieldSignature
  "ArrayFieldExample"
  (TArray TInt)
  "arr"

arrayFieldExampleArrField :: Field
arrayFieldExampleArrField = Field {
  fieldModifiers = [Public],
  fieldType = TArray TInt,
  fieldName = "arr"
}

arrayFieldExampleInitSignature :: MethodSignature
arrayFieldExampleInitSignature = MethodSignature
  "ArrayFieldExample"
  TVoid
  "<init>"
  []

arrayFieldExampleInitMethod :: Method
arrayFieldExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "ArrayFieldExample", ["r0"])
    ],
    statements = [
      Identity "r0" IDThis (TClass "ArrayFieldExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

arrayFieldExampleMainMethod :: Method
arrayFieldExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = TVoid,
  methodName = "main",
  parameters = [
    TInt
  ],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TInt, ["r0"]),
      (TArray TInt, ["r1"]),
      (TClass "ArrayFieldExample", ["$r2"])
    ],
    statements = [
      Identity "r0" (IDParameter 0) (TInt),
      Assign (VLocal "$r2") (ENew (NewSimple (TClass "ArrayFieldExample"))),
      Invoke (SpecialInvoke "$r2" arrayFieldExampleInitSignature []),
      Invoke (VirtualInvoke "$r2" arrayFieldExampleFillSignature [ILocalName "r0"]),
      Assign (VLocal "r1") (EReference (FieldReference "$r2" arrayFieldExampleArrSignature)),
      Return (Just (ILocalName "r1"))
    ],
    catchClauses = []
  }
}

arrayFieldExampleFillSignature :: MethodSignature
arrayFieldExampleFillSignature = MethodSignature
  "ArrayFieldExample"
  TVoid
  "fill"
  [TInt]

arrayFieldExampleFillMethod :: Method
arrayFieldExampleFillMethod = Method {
  methodModifiers = [Private],
  returnType = TVoid,
  methodName = "fill",
  parameters = [TInt],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "ArrayFieldExample", ["r0"]),
      (TInt, ["p0"]),
      (TArray TInt, ["a0"])
    ],
    statements = [
      Identity "r0" IDThis (TClass "ArrayFieldExample"),
      Identity "p0" (IDParameter 0) TInt,
      Assign (VLocal "a0") (ENew (NewArray TInt (IInt 4))),
      Assign (VReference (ArrayReference "a0" (IInt 0))) (EImmediate (ILocalName "p0")),
      Assign (VReference (ArrayReference "a0" (IInt 1))) (EImmediate (ILocalName "p0")),
      Assign (VReference (ArrayReference "a0" (IInt 2))) (EImmediate (ILocalName "p0")),
      Assign (VReference (ArrayReference "a0" (IInt 3))) (EImmediate (ILocalName "p0")),
      Assign (VReference (FieldReference "r0" arrayFieldExampleArrSignature)) (EImmediate (ILocalName "a0")),
      Return Nothing
    ],
    catchClauses = []
  }
}

arrayFieldExampleFile :: File
arrayFieldExampleFile = File {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "ArrayFieldExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember arrayFieldExampleArrField,
    MethodMember arrayFieldExampleInitMethod,
    MethodMember arrayFieldExampleMainMethod,
    MethodMember arrayFieldExampleFillMethod
  ]
}

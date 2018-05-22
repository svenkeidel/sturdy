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
      Identity "r0" ThisRef (TClass "ArrayFieldExample"),
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
      Identity "r0" (ParameterRef 0) (TInt),
      Assign (VLocal "$r2") (NewExpr (TClass "ArrayFieldExample")),
      Invoke (SpecialInvoke "$r2" arrayFieldExampleInitSignature []),
      Invoke (VirtualInvoke "$r2" arrayFieldExampleFillSignature [Local "r0"]),
      Assign (VLocal "r1") (FieldRef "$r2" arrayFieldExampleArrSignature),
      Return (Just (Local "r1"))
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
      Identity "r0" ThisRef (TClass "ArrayFieldExample"),
      Identity "p0" (ParameterRef 0) TInt,
      Assign (VLocal "a0") (NewArrayExpr TInt (IntConstant 4)),
      Assign (VReference (ArrayRef "a0" (IntConstant 0))) (Local "p0"),
      Assign (VReference (ArrayRef "a0" (IntConstant 1))) (Local "p0"),
      Assign (VReference (ArrayRef "a0" (IntConstant 2))) (Local "p0"),
      Assign (VReference (ArrayRef "a0" (IntConstant 3))) (Local "p0"),
      Assign (VReference (FieldRef "r0" arrayFieldExampleArrSignature)) (Local "a0"),
      Return Nothing
    ],
    catchClauses = []
  }
}

arrayFieldExampleFile :: CompilationUnit
arrayFieldExampleFile = CompilationUnit {
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

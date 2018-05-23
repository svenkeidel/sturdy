module Classes.PersonExample where

import Syntax

import Classes.Object

personExampleAgeSignature :: FieldSignature
personExampleAgeSignature = FieldSignature
  "PersonExample"
  IntType
  "age"

personExampleAgeField :: Field
personExampleAgeField = Field {
  fieldModifiers = [Public],
  fieldType = IntType,
  fieldName = "age"
}

personExampleMaxAgeSignature :: FieldSignature
personExampleMaxAgeSignature = FieldSignature
  "PersonExample"
  IntType
  "MAX_AGE"

personExampleMaxAgeField :: Field
personExampleMaxAgeField = Field {
  fieldModifiers = [Public, Static],
  fieldType = IntType,
  fieldName = "MAX_AGE"
}

personExampleClassInitMethod :: Method
personExampleClassInitMethod = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "<clinit>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [],
    statements = [
      Assign (VReference (SignatureRef personExampleMaxAgeSignature)) (IntConstant 100)
    ],
    catchClauses = []
  }
}

personExampleMainMethod :: Method
personExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = IntType,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (IntType, ["r0"]),
      (RefType "PersonExample", ["$r2"])
    ],
    statements = [
      Assign (VLocal "$r2") (NewExpr (RefType "PersonExample")),
      Invoke (SpecialInvoke "$r2" personExampleInitSignature [IntConstant 10]),
      Assign (VLocal "r0") (InvokeExpr (VirtualInvoke "$r2" personExampleYearsLeftSignature [])),
      Return (Just (Local "r0"))
    ],
    catchClauses = []
  }
}

personExampleInitSignature :: MethodSignature
personExampleInitSignature = MethodSignature
  "PersonExample"
  VoidType
  "<init>"
  [IntType]

personExampleInitMethod :: Method
personExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [IntType],
  throws = [],
  methodBody = MFull {
    declarations = [
      (RefType "PersonExample", ["r0"]),
      (IntType, ["i0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "PersonExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Identity "i0" (ParameterRef 0) IntType,
      Assign (VReference (FieldRef "r0" personExampleAgeSignature)) (Local "i0"),
      Return Nothing
    ],
    catchClauses = []
  }
}

personExampleYearsLeftSignature :: MethodSignature
personExampleYearsLeftSignature = MethodSignature
  "PersonExample"
  IntType
  "yearsLeft"
  [IntType]

personExampleYearsLeftMethod :: Method
personExampleYearsLeftMethod = Method {
  methodModifiers = [Private],
  returnType = IntType,
  methodName = "yearsLeft",
  parameters = [IntType],
  throws = ["java.lang.IllegalArgumentException"],
  methodBody = MFull {
    declarations = [
      (RefType "PersonExample", ["r0"]),
      (IntType, ["i0", "i1", "i2"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "PersonExample"),
      Assign (VLocal "i0") (FieldRef "r0" personExampleAgeSignature),
      Assign (VLocal "i1") (SignatureRef personExampleMaxAgeSignature),
      Assign (VLocal "i2") (BinopExpr (Local "i1") Minus (Local "i0")),
      Return (Just (Local "i2"))
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

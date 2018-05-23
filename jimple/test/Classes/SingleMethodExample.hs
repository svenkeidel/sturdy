module Classes.SingleMethodExample where

import Syntax

import Classes.Object

singleMethodExampleXSignature :: FieldSignature
singleMethodExampleXSignature = FieldSignature
  "SingleMethodExample"
  IntType
  "x"

singleMethodExampleXField :: Field
singleMethodExampleXField = Field {
  fieldModifiers = [Public],
  fieldType = IntType,
  fieldName = "x"
}

singleMethodExampleInitSignature :: MethodSignature
singleMethodExampleInitSignature = MethodSignature
  "SingleMethodExample"
  VoidType
  "<init>"
  []

singleMethodExampleInitMethod :: Method
singleMethodExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (RefType "SingleMethodExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "SingleMethodExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

singleMethodExampleMainMethod :: Method
singleMethodExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (IntType, ["i0"]),
      (RefType "SingleMethodExample", ["$r2"])
    ],
    statements = [
      Assign (VLocal "$r2") (NewExpr (RefType "SingleMethodExample")),
      Invoke (VirtualInvoke "$r2" singleMethodExampleFooSignature []),
      Assign (VLocal "i0") (FieldRef "$r2" singleMethodExampleXSignature),
      Return (Just (Local "i0"))
    ],
    catchClauses = []
  }
}

singleMethodExampleFooSignature :: MethodSignature
singleMethodExampleFooSignature = MethodSignature
  "SingleMethodExample"
  VoidType
  "foo"
  []

singleMethodExampleFooMethod :: Method
singleMethodExampleFooMethod = Method {
  methodModifiers = [Private],
  returnType = VoidType,
  methodName = "foo",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (RefType "SingleMethodExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "SingleMethodExample"),
      Assign (VReference (FieldRef "r0" singleMethodExampleXSignature)) (IntConstant 2),
      Return Nothing
    ],
    catchClauses = []
  }
}

singleMethodExampleFile :: CompilationUnit
singleMethodExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "SingleMethodExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember singleMethodExampleXField,
    MethodMember singleMethodExampleInitMethod,
    MethodMember singleMethodExampleMainMethod,
    MethodMember singleMethodExampleFooMethod
  ]
}

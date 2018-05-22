module Classes.SingleMethodExample where

import Syntax

import Classes.Object

singleMethodExampleXSignature :: FieldSignature
singleMethodExampleXSignature = FieldSignature
  "SingleMethodExample"
  TInt
  "x"

singleMethodExampleXField :: Field
singleMethodExampleXField = Field {
  fieldModifiers = [Public],
  fieldType = TInt,
  fieldName = "x"
}

singleMethodExampleInitSignature :: MethodSignature
singleMethodExampleInitSignature = MethodSignature
  "SingleMethodExample"
  TVoid
  "<init>"
  []

singleMethodExampleInitMethod :: Method
singleMethodExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "SingleMethodExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "SingleMethodExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

singleMethodExampleMainMethod :: Method
singleMethodExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = TVoid,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TInt, ["i0"]),
      (TClass "SingleMethodExample", ["$r2"])
    ],
    statements = [
      Assign (VLocal "$r2") (NewExpr (TClass "SingleMethodExample")),
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
  TVoid
  "foo"
  []

singleMethodExampleFooMethod :: Method
singleMethodExampleFooMethod = Method {
  methodModifiers = [Private],
  returnType = TVoid,
  methodName = "foo",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "SingleMethodExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "SingleMethodExample"),
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

module Classes.SingleMethodExample where

import Syntax

import Java.Lang.Object

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
  methodBody = FullBody {
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
  methodBody = FullBody {
    declarations = [
      (IntType, ["i0"]),
      (RefType "SingleMethodExample", ["$r2"])
    ],
    statements = [
      Assign (LocalVar "$r2") (NewExpr (RefType "SingleMethodExample")),
      Invoke (VirtualInvoke "$r2" singleMethodExampleFooSignature []),
      Assign (LocalVar "i0") (RefExpr (FieldRef "$r2" singleMethodExampleXSignature)),
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
  methodBody = FullBody {
    declarations = [
      (RefType "SingleMethodExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "SingleMethodExample"),
      Assign (ReferenceVar (FieldRef "r0" singleMethodExampleXSignature)) (ImmediateExpr (IntConstant 2)),
      Return Nothing
    ],
    catchClauses = []
  }
}

singleMethodExampleFile :: CompilationUnit
singleMethodExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
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

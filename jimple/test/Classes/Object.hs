module Classes.Object where

import Syntax

objectInitSignature :: MethodSignature
objectInitSignature = MethodSignature
  "java.lang.Object"
  VoidType
  "<init>"
  []

objectInitMethod :: Method
objectInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [],
    statements = [
      Return Nothing
    ],
    catchClauses = []
  }
}

objectFile :: CompilationUnit
objectFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "java.lang.Object",
  extends = Nothing,
  implements = [],
  fileBody = [
    MethodMember objectInitMethod
  ]
}

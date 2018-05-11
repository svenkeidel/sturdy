module Classes.Object where

import Syntax

objectInitSignature :: MethodSignature
objectInitSignature = MethodSignature
  "java.lang.Object"
  TVoid
  "<init>"
  []

objectInitMethod :: Method
objectInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
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

objectFile :: File
objectFile = File {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "java.lang.Object",
  extends = Nothing,
  implements = [],
  fileBody = [
    MethodMember objectInitMethod
  ]
}

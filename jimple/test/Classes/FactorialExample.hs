module Classes.FactorialExample where

import Syntax

import Classes.Object
import Classes.IllegalArgumentException

factorialExampleInitSignature :: MethodSignature
factorialExampleInitSignature = MethodSignature
  "FactorialExample"
  TVoid
  "<init>"
  []

factorialExampleInitMethod :: Method
factorialExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TClass "FactorialExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "FactorialExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

factorialExampleMainMethod :: Method
factorialExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = TVoid,
  methodName = "main",
  parameters = [
    TInt
  ],
  throws = [],
  methodBody = MFull {
    declarations = [
      (TInt, ["r0", "r1"]),
      (TClass "FactorialExample", ["$r2"])
    ],
    statements = [
      Identity "r0" (ParameterRef 0) (TInt),
      Assign (VLocal "$r2") (NewExpr (TClass "FactorialExample")),
      Invoke (SpecialInvoke "$r2" factorialExampleInitSignature []),
      Assign (VLocal "r1") (InvokeExpr (VirtualInvoke "$r2" factorialExampleExecSignature [Local "r0"])),
      Return (Just (Local "r1"))
    ],
    catchClauses = []
  }
}

factorialExampleExecSignature :: MethodSignature
factorialExampleExecSignature = MethodSignature
  "FactorialExample"
  TInt
  "exec"
  [TInt]

factorialExampleExecMethod :: Method
factorialExampleExecMethod = Method {
  methodModifiers = [Private],
  returnType = TInt,
  methodName = "exec",
  parameters = [TInt],
  throws = ["java.lang.IllegalArgumentException"],
  methodBody = MFull {
    declarations = [
      (TClass "FactorialExample", ["r0"]),
      (TInt, ["i0", "$i1", "$i2", "$i3"]),
      (TClass "java.lang.IllegalArgumentException", ["$r1"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "FactorialExample"),
      Identity "i0" (ParameterRef 0) TInt,
      If (BinopExpr (Local "i0") Cmpge (IntConstant 0)) "label1",
      Assign (VLocal "$r1") (NewExpr (TClass "java.lang.IllegalArgumentException")),
      Invoke (SpecialInvoke "$r1" illegalArgumentExceptionInitSignature [StringConstant "Negative value for argument n"]),
      Throw (Local "$r1"),
      Label "label1",
      If (BinopExpr (Local "i0") Cmpne (IntConstant 0)) "label2",
      Return (Just (IntConstant 1)),
      Label "label2",
      Assign (VLocal "$i1") (BinopExpr (Local "i0") Minus (IntConstant 1)),
      Assign (VLocal "$i2") (InvokeExpr (SpecialInvoke "r0" factorialExampleExecSignature [Local "$i1"])),
      Assign (VLocal "$i3") (BinopExpr (Local "i0") Mult (Local "$i2")),
      Return (Just (Local "$i3"))
    ],
    catchClauses = []
  }
}

factorialExampleFile :: CompilationUnit
factorialExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "FactorialExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    MethodMember factorialExampleInitMethod,
    MethodMember factorialExampleMainMethod,
    MethodMember factorialExampleExecMethod
  ]
}

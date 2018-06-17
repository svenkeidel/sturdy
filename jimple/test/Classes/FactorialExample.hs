module Classes.FactorialExample where

import Syntax

import Classes.Object
import Classes.IllegalArgumentException

factorialExampleInitSignature :: MethodSignature
factorialExampleInitSignature = MethodSignature
  "FactorialExample"
  VoidType
  "<init>"
  []

factorialExampleInitMethod :: Method
factorialExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "FactorialExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "FactorialExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

factorialExampleMainMethod :: Method
factorialExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "main",
  parameters = [
    IntType
  ],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (IntType, ["r0", "r1"]),
      (RefType "FactorialExample", ["$r2"])
    ],
    statements = [
      Identity "r0" (ParameterRef 0) IntType,
      Assign (LocalVar "$r2") (NewExpr (RefType "FactorialExample")),
      Invoke (SpecialInvoke "$r2" factorialExampleInitSignature []),
      Assign (LocalVar "r1") (InvokeExpr (VirtualInvoke "$r2" factorialExampleExecSignature [Local "r0"])),
      Return (Just (Local "r1"))
    ],
    catchClauses = []
  }
}

factorialExampleExecSignature :: MethodSignature
factorialExampleExecSignature = MethodSignature
  "FactorialExample"
  IntType
  "exec"
  [IntType]

factorialExampleExecMethod :: Method
factorialExampleExecMethod = Method {
  methodModifiers = [Private],
  returnType = IntType,
  methodName = "exec",
  parameters = [IntType],
  throws = ["java.lang.IllegalArgumentException"],
  methodBody = FullBody {
    declarations = [
      (RefType "FactorialExample", ["r0"]),
      (IntType, ["i0", "$i1", "$i2", "$i3"]),
      (RefType "java.lang.IllegalArgumentException", ["$r1"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "FactorialExample"),
      Identity "i0" (ParameterRef 0) IntType,
      If (BoolExpr (Local "i0") Cmpge (IntConstant 0)) "label1",
      Assign (LocalVar "$r1") (NewExpr (RefType "java.lang.IllegalArgumentException")),
      Invoke (SpecialInvoke "$r1" illegalArgumentExceptionInitSignature [StringConstant "Negative value for argument n"]),
      Throw (Local "$r1"),
      Label "label1",
      If (BoolExpr (Local "i0") Cmpne (IntConstant 0)) "label2",
      Return (Just (IntConstant 1)),
      Label "label2",
      Assign (LocalVar "$i1") (BinopExpr (Local "i0") Minus (IntConstant 1)),
      Assign (LocalVar "$i2") (InvokeExpr (SpecialInvoke "r0" factorialExampleExecSignature [Local "$i1"])),
      Assign (LocalVar "$i3") (BinopExpr (Local "i0") Mult (Local "$i2")),
      Return (Just (Local "$i3"))
    ],
    catchClauses = []
  }
}

factorialExampleFile :: CompilationUnit
factorialExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "FactorialExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    MethodMember factorialExampleInitMethod,
    MethodMember factorialExampleMainMethod,
    MethodMember factorialExampleExecMethod
  ]
}

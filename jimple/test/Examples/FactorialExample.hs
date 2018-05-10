module Examples.FactorialExample where

import Syntax

initIllegalArgumentExceptionSignature :: MethodSignature
initIllegalArgumentExceptionSignature = MethodSignature
  "java.lang.IllegalArgumentException"
  TVoid
  "<init>"
  []

objectInitSignature :: MethodSignature
objectInitSignature = MethodSignature
  "java.lang.Object"
  TVoid
  "<init>"
  []

initSignature :: MethodSignature
initSignature = MethodSignature
  "FactorialExample"
  TVoid
  "<init>"
  []

initMethod :: Method
initMethod = Method {
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
      Identity "r0" IDThis (TClass "FactorialExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

mainMethod :: Method
mainMethod = Method {
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
      Identity "r0" (IDParameter 0) (TInt),
      Assign (VLocal "$r2") (ENew (NewSimple (TClass "FactorialExample"))),
      Invoke (SpecialInvoke "$r2" initSignature []),
      Assign (VLocal "r1") (EInvoke (VirtualInvoke "$r2" execSignature [ILocalName "r0"])),
      Return (Just (ILocalName "r1"))
    ],
    catchClauses = []
  }
}

execSignature :: MethodSignature
execSignature = MethodSignature
  "FactorialExample"
  TInt
  "exec"
  [TInt]

execMethod :: Method
execMethod = Method {
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
      Identity "r0" IDThis (TClass "FactorialExample"),
      Identity "i0" (IDParameter 0) TInt,
      If (EBinop (ILocalName "i0") Cmpge (IInt 0)) "label1",
      Assign (VLocal "$r1") (ENew (NewSimple (TClass "java.lang.IllegalArgumentException"))),
      Invoke (SpecialInvoke "$r1" initIllegalArgumentExceptionSignature [IString "Negative value for argument n"]),
      Throw (ILocalName "$r1"),
      Label "label1",
      If (EBinop (ILocalName "i0") Cmpne (IInt 0)) "label2",
      Return (Just (IInt 1)),
      Label "label2",
      Assign (VLocal "$i1") (EBinop (ILocalName "i0") Minus (IInt 1)),
      Assign (VLocal "$i2") (EInvoke (SpecialInvoke "r0" execSignature [ILocalName "$i1"])),
      Assign (VLocal "$i3") (EBinop (ILocalName "i0") Mult (ILocalName "$i2")),
      Return (Just (ILocalName "$i3"))
    ],
    catchClauses = []
  }
}

file :: File
file = File {
  fileModifiers = [Public],
  fileType = FTClass,
  fileName = "FactorialExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    MethodMember initMethod,
    MethodMember mainMethod,
    MethodMember execMethod
  ]
}

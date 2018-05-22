module SimpleExampleSpec where

import Syntax

objectInitSignature = MethodSignature
  "java.lang.Object"
  TVoid
  "<init>"
  []

printStreamSignature = FieldSignature
  "java.lang.System"
  (TClass "java.io.PrintStream")
  "out"

printlnSignature = MethodSignature
  "java.io.PrintStream"
  TVoid
  "println"
  [TClass "java.lang.Object"]

initSignature = MethodSignature
  "SimpleExample"
  TVoid
  "<init>"
  []

initMethod = Method {
  modifiers = [Public],
  returnType = TVoid,
  methodName = "<init>",
  parameters = [],
  throws = [],
  body = MFull {
    declarations = [
      (TClass "SimpleExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "SimpleExample"),
      Invoke SpecialInvoke "r0" objectInitSignature [],
      Return Nothing
    ],
    catchClauses = []
  }
}

mainMethod = Method {
  modifiers = [Public, Static],
  returnType = TVoid,
  methodName = "main",
  parameters = [
    TArray (TClass "java.lang.String")
  ],
  throws = [],
  body = MFull {
    declarations = [
      (TArray (TClass "java.lang.String"), ["r0"]),
      (TClass "SimpleExample", ["$r2"])
    ],
    statements = [
      Identity "r0" (ParameterRef 0) (TClass "java.lang.String"),
      Assign VLocal "$r2" ENew NewSimple TClass "SimpleExample",
      Invoke SpecialInvoke "$r2" initSignature [],
      Invoke VirtualInvoke "$r2" fooSignature [],
      Return Nothing
    ],
    catchClauses = []
  }
}

fooSignature = MethodSignature {
  className = "SimpleExample",
  returnType = TVoid,
  methodName = "foo",
  parameters = []
}

fooMethod = Method {
  modifiers = [Public],
  returnType = TVoid,
  methodName = "foo",
  parameters = [],
  throws = [],
  body = MFull {
    declarations = [
      (TClass "SimpleExample", ["r0"]),
      (TArray TInt, ["r1", "r2", "r3"]),
      (TInt, ["i1", "i2", "$i3", "$i4", "$i5", "$i6", "$i7",
              "$i8", "$i9", "$i10", "$i11", "$i12", "i13", "i14"]),
      (TClass "java.io.PrintStream", ["$r5"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "SimpleExample"),
      Assign (VLocal "r1") (ENew NewArray TInt (IInt 8)),
      Assign (VLocal "r2") (ENew NewArray TInt (IInt 8)),
      Assign (VLocal "i13") (EImmediate IInt 0),
      Assign (VLocal "i14") (EImmediate IInt 0),
      Label "label1",
      Assign (VLocal "$i2") (BinopExpr (IInt 8) Div (IInt 2)),
      Assign (VLocal "r3") (ENew NewArray TInt (Local "$i2")),
      Label "label2",
      If (BinopExpr (Local "i13") Cmpge (IInt 8)) "label3",
      Assign (VLocal "$i5") (EReference ArrayReference "r1" Local "i13"),
      Assign (VLocal "$i3") (BinopExpr (Local "i13") Plus (IInt 1)),
      Assign (VLocal "$i4") (EReference ArrayReference "r1" Local "$i3"),
      Assign (VLocal "$i1") (EInvoke VirtualInvoke "r0" ltSignature [
        Local "$i5",
        Local "$i4"
      ]),
      Assign (VLocal "$i6") (EReference ArrayReference "r1" Local "i13"),
      Assign (VLocal "$i11") (BinopExpr (Local "i1") Mult (Local "$i6")),
      Assign (VLocal "$i9") (BinopExpr (IInt 1) Minus (Local "i1")),
      Assign (VLocal "$i7") (BinopExpr (Local "i13") Plus (IInt 1)),
      Assign (VLocal "$i8") (EReference ArrayReference "r1" Local "$i7"),
      Assign (VLocal "$i10") (BinopExpr (Local "$i9") Mult (Local "$i8")),
      Assign (VLocal "$i12") (BinopExpr (Local "$i11") Plus (Local "$i10")),
      Assign (VReference ArrayReference "r3" Local "i14") EImmediate Local "$i12",
      Assign (VLocal "i13") (BinopExpr (Local "i13") Plus (IInt 2)),
      Assign (VLocal "i14") (BinopExpr (Local "i14") Plus (IInt 1)),
      Goto "label2",
      Label "label3",
      Assign (VLocal "$r5") EReference SignatureReference printStreamSignature,
      Invoke VirtualInvoke "$r5" printlnSignature [Local "r3"],
      Goto "label1"
    ],
    catchClauses = []
  }
}

ltSignature = MethodSignature {
  className = "SimpleExample",
  returnType = TInt,
  methodName = "lt",
  parameters = [TInt, TInt]
}

ltMethod = Method {
  modifiers = [Public],
  returnType = TInt,
  methodName = "lt",
  parameters = [TInt, TInt],
  throws = [],
  body = MFull {
    declarations = [
      (TClass "SimpleExample", ["r0"]),
      (TInt, ["i0", "i1"])
    ],
    statements = [
      Identity "r0" ThisRef (TClass "SimpleExample"),
      Identity "i0" (ParameterRef 0) TInt,
      Identity "i1" (ParameterRef 1) TInt,
      If (BinopExpr (Local "i0") Cmple (Local "i1")) "label1",
      Return (Just IInt 1),
      Label "label1",
      Return (Just IInt 0)
    ],
    catchClauses = []
  }
}

file = File {
  modifiers = [Public],
  fileType = FTClass,
  className = "SimpleExample",
  extends = Just "java.lang.Object",
  implements = Nothing,
  body = [
    initMethod,
    mainMethod,
    fooMethod,
    ltMethod
  ]
}

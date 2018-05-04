module SimpleExampleSpec where

import Syntax

objectInitSignature = MethodSignature {
  className = "java.lang.Object",
  returnType = TVoid,
  methodName = "<init>",
  parameters = Nothing
}

printStreamSignature = FieldSignature {
  className: "java.lang.System",
  fieldType: TClass "java.io.PrintStream",
  fieldName: "out",
}

printlnSignature = MethodSignature {
  className = "java.io.PrintStream",
  returnType = TVoid,
  methodName = "println",
  parameters = Just [TClass "java.lang.Object"]
}

initSignature = MethodSignature {
  className = "SimpleExample",
  returnType = TVoid,
  methodName = "<init>",
  parameters = Nothing
}

initMethod = Method {
  modifiers = [Public],
  returnType = TVoid,
  name = "<init>",
  parameters = Nothing,
  throws = Nothing,
  body = MFull {
    declarations = [
      (TClass "SimpleExample", ["r0"])
    ],
    statements = [
      Identity "r0" (IDThis) (TClass "SimpleExample"),
      Invoke EInvoke SpecialInvoke "r0" objectInitSignature Nothing,
      Return Nothing
    ],
    catchClauses = [],
  }
}

mainMethod = Method {
  modifiers = [Public, Static],
  returnType = TVoid,
  name = "main",
  parameters = Just [
    TArray (TClass "java.lang.String")
  ],
  throws = Nothing,
  body = MFull {
    declarations = [
      (TArray (TClass "java.lang.String"), ["r0"]),
      (TClass "SimpleExample", ["$r2"])
    ],
    statements = [
      Identity "r0" (IDParameter 0) (TClass "java.lang.String"),
      Assign VLocal "$r2" ENew NewSimple TClass "SimpleExample",
      Invoke EInvoke SpecialInvoke "$r2" initSignature Nothing,
      Invoke EInvoke VirtualInvoke "$r2" fooSignature Nothing,
      Return Nothing
    ],
    catchClauses = [],
  }
}

fooSignature = MethodSignature {
  className = "SimpleExample",
  returnType = TVoid,
  methodName = "foo",
  parameters = Nothing
}

fooMethod = Method {
  modifiers = [Public],
  returnType = TVoid,
  name = "foo",
  parameters = Nothing,
  throws = Nothing,
  body = MFull {
    declarations = [
      (TClass "SimpleExample", ["r0"]),
      (TArray TInt, ["r1", "r2", "r3"]),
      (TInt, ["i1", "i2", "$i3", "$i4", "$i5", "$i6", "$i7",
              "$i8", "$i9", "$i10", "$i11", "$i12", "i13", "i14"]),
      (TClass "java.io.PrintStream", ["$r5"]),
    ],
    statements = [
      Identity "r0" (IDThis) (TClass "SimpleExample"),
      Assign (VLocal "r1") (ENew NewArray TInt (IInt 8)),
      Assign (VLocal "r2") (ENew NewArray TInt (IInt 8)),
      Assign (VLocal "i13") (EImmediate IInt 0),
      Assign (VLocal "i14") (EImmediate IInt 0),
      Label "label1",
      Assign (VLocal "$i2") (EBinop (IInt 8) Div (IInt 2)),
      Assign (VLocal "r3") (ENew NewArray TInt (ILocalName "$i2")),
      Label "label2",
      If (EBinop (ILocalName "i13") Cmpge (IInt 8)) "label3",
      Assign (VLocal "$i5") (EReference ArrayReference "r1" ILocalName "i13"),
      Assign (VLocal "$i3") (EBinop (ILocalName "i13") Plus (IInt 1)),
      Assign (VLocal "$i4") (EReference ArrayReference "r1" ILocalName $"i3"),
      Assign (VLocal "$i1") (EInvoke VirtualInvoke "r0" ltSignature Just [
        ILocalName "$i5",
        ILocalName "$i4"
      ]),
      Assign (VLocal "$i6") (EReference ArrayReference "r1" ILocalName "i13"),
      Assign (VLocal "$i11") (EBinop (ILocalName "i1") Mult (ILocalName "$i6")),
      Assign (VLocal "$i9") (EBinop (IInt 1) Minus (ILocalName "i1")),
      Assign (VLocal "$i7") (EBinop (ILocalName "i13") Plus (IInt 1)),
      Assign (VLocal "$i8") (EReference ArrayReference "r1" ILocalName "$i7"),
      Assign (VLocal "$i10") (EBinop (ILocalName "$i9") Mult (ILocalName "$i8")),
      Assign (VLocal "$i12") (EBinop (ILocalName "$i11") Plus (ILocalName "$i10")),
      Assign (VReference ArrayReference "r3" ILocalName "i14") EImmediate ILocalName "$i12",
      Assign (VLocal "i13") (EBinop (ILocalName "i13") Plus (IInt 2)),
      Assign (VLocal "i14") (EBinop (ILocalName "i14") Plus (IInt 1)),
      Goto "label2",
      Label "label3",
      Assign (VLocal "$r5") EReference SignatureReference printStreamSignature,
      Invoke VirtualInvoke "$r5" printlnSignature, Just ILocalName "r3",
      Goto "label1"
    ],
    catchClauses = [],
  },
}

ltSignature = MethodSignature {
  className = "SimpleExample",
  returnType = TInt,
  methodName = "lt",
  parameters = Just [TInt, TInt]
}

ltMethod = Method {
  modifiers = [Public],
  returnType = TInt,
  name = "lt",
  parameters = Just [TInt, TInt],
  throws = Nothing,
  body = MFull {
    declarations = [
      (TClass "SimpleExample", ["r0"]),
      (TInt, ["i0", "i1"])
    ],
    statements = [
      Identity "r0" (IDThis) (TClass "SimpleExample"),
      Identity "i0" (IDParameter 0) (TInt),
      Identity "i1" (IDParameter 1) (TInt),
      If (EBinop (ILocalName "i0") Cmple (ILocalName "i1")) "label1",
      Return (Just IInt 1),
      Label "label1",
      Return (Just IInt 0)
    ],
    catchClauses = [],
  },
}

fileBody = [
  initMethod,
  mainMethod,
  fooMethod,
  ltMethod,
]

file = File {
  modifiers = [Public],
  fileType = FTClass,
  className = "SimpleExample",
  extends = Just "java.lang.Object",
  implements = Nothing,
  body = fileBody
}

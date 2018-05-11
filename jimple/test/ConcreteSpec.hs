module ConcreteSpec where

import Concrete
import Syntax
import Data.Concrete.Error

import qualified Data.Map as Map

import Test.Hspec

import Classes.FactorialExample
import Classes.IllegalArgumentException
import Classes.Object

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Literals" $ do
    it "LocalName lookup" $ do
      let expr = EImmediate (ILocalName "x")
      let nv = [("x", LocalAddr 1)]
      let st = [(LocalAddr 1, VariableVal (TInt, VInt 2))]
      evalConcrete nv st expr `shouldBe` Success (VInt 2)
    it "Integer literals" $ do
      let expr = EImmediate (IInt 7)
      evalConcrete env store expr `shouldBe` Success (VInt 7)
    it "Float literals" $ do
      let expr = EImmediate (IFloat 2.5)
      evalConcrete env store expr `shouldBe` Success (VFloat 2.5)
    it "String literals" $ do
      let expr = EImmediate (IString "Hello World")
      evalConcrete env store expr `shouldBe` Success (VString "Hello World")
    it "Class literals" $ do
      let expr = EImmediate (IClass "java.lang.Object")
      evalConcrete env store expr `shouldBe` Success (VClass "java.lang.Object")
    it "Null literals" $ do
      let expr = EImmediate (INull)
      evalConcrete env store expr `shouldBe` Success (VNull)

  describe "Simple Expressions" $ do
    it "-3" $ do
      let expr = EUnop Neg (IInt 3)
      evalConcrete env store expr `shouldBe` Success (VInt (-3))
    it "lengthof [1, 2, 3]" $ do
      let expr = EUnop Lengthof (ILocalName "x")
      let nv = [("x", LocalAddr 1)]
      let st = [(LocalAddr 1, VariableVal (TArray TInt, VArray [VInt 1, VInt 2, VInt 3]))]
      evalConcrete nv st expr `shouldBe` Success (VInt 3)
    it "8 + 2" $ do
      let expr = EBinop (IInt 8) Plus (IInt 2)
      evalConcrete env store expr `shouldBe` Success (VInt 10)
    it "8 / 0" $ do
      let expr = EBinop (IInt 8) Div (IInt 0)
      evalConcrete env store expr `shouldBe` Fail "Cannot divide by zero"
    it "3 < 3.5" $ do
      let expr = EBinop (IInt 3) Cmplt (IFloat 3.5)
      evalConcrete env store expr `shouldBe` Success (VBool True)
    it "3 != 'three'" $ do
      let expr = EBinop (IInt 3) Cmpne (IString "three")
      evalConcrete env store expr `shouldBe` Success (VBool True)
    it "3 % 2.5" $ do
      let expr = EBinop (IInt 3) Mod (IFloat 2.5)
      evalConcrete env store expr `shouldBe` Success (VFloat 0.5)
    it "new boolean" $ do
      let expr = ENew (NewSimple TBoolean)
      evalConcrete env store expr `shouldBe` Success (VInt 0)
    it "[1, 2, 3][2]" $ do
      let expr = EReference (ArrayReference "xs" (IInt 2))
      let nv = [("xs", LocalAddr 1)]
      let st = [(LocalAddr 1, VariableVal (TArray TInt, VArray [VInt 1, VInt 2, VInt 3]))]
      evalConcrete nv st expr `shouldBe` Success (VInt 3)
    it "p.<Person: int age>" $ do
      let personAgeSignature = FieldSignature "Person" TInt "age"
      let personObject = Map.fromList [("age", VInt 10)]
      let nv = [("p", LocalAddr 1)]
      let st = [(LocalAddr 1, VariableVal (TClass "Person", VObject "Person" personObject))]
      let expr = EReference (FieldReference "p" personAgeSignature)
      evalConcrete nv st expr `shouldBe` Success (VInt 10)
    it "<Person: int MAX_AGE>" $ do
      let maxAgeSignature = FieldSignature "Person" TInt "MAX_AGE"
      let st = [(FieldAddr maxAgeSignature, VariableVal (TInt, VInt 100))]
      let expr = EReference (SignatureReference maxAgeSignature)
      evalConcrete env st expr `shouldBe` Success (VInt 100)
    it "newmultiarray (float) [3][]" $ do
      let expr = ENew (NewMulti TFloat [IInt 3, IInt 2])
      evalConcrete env store expr `shouldBe` Success (VArray [VArray [VFloat 0.0, VFloat 0.0],
                                                 VArray [VFloat 0.0, VFloat 0.0],
                                                 VArray [VFloat 0.0, VFloat 0.0]])

  describe "Simple Statements" $ do
    it "i0 = 2 + 3; return i0;" $ do
      let nv = [("i0", LocalAddr 1)]
      let st = [(LocalAddr 1, VariableVal (TInt, defaultValue TInt))]
      let stmts = [Assign (VLocal "i0") (EBinop (IInt 2) Plus (IInt 3)),
                   Return (Just (ILocalName "i0"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (VInt 5))
    it "s = 2; xs = newarray (int)[s]; return xs;" $ do
      let nv = [("s", LocalAddr 0),
                ("xs", LocalAddr 1)]
      let st = [(LocalAddr 0, VariableVal (TInt, defaultValue TInt)),
                (LocalAddr 1, VariableVal (TArray TInt, defaultValue (TArray TInt)))]
      let stmts = [Assign (VLocal "s") (EImmediate (IInt 2)),
                   Assign (VLocal "xs") (ENew (NewArray TInt (ILocalName "s"))),
                   Return (Just (ILocalName "xs"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (VArray [VInt 0, VInt 0]))
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let stmts = [If (EBinop (IInt 2) Cmple (IInt 3)) "l2",
                   Label "l1",
                   Return (Just (IInt 1)),
                   Label "l2",
                   Return (Just (IInt 0))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (VInt 0))
    it "lookupswitch(4) { case 0: goto l1; case 4: goto l2; default: goto l3;}; l1: return 1; l2: return 2; l3: return 3;" $ do
      let stmts = [Lookupswitch (IInt 4) [(CLConstant 0, "l1"),
                                          (CLConstant 4, "l2"),
                                          (CLDefault, "l3")],
                   Label "l1",
                   Return (Just (IInt 1)),
                   Label "l2",
                   Return (Just (IInt 2)),
                   Label "l3",
                   Return (Just (IInt 3))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (VInt 2))
    it "lookupswitch(2) { case 0: goto l1; case 4: goto l2; default: goto l3;}; l1: return 1; l2: return 2; l3: return 3;" $ do
      let stmts = [Lookupswitch (IInt 2) [(CLConstant 0, "l1"),
                                          (CLConstant 4, "l2"),
                                          (CLDefault, "l3")],
                   Label "l1",
                   Return (Just (IInt 1)),
                   Label "l2",
                   Return (Just (IInt 2)),
                   Label "l3",
                   Return (Just (IInt 3))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (VInt 3))
    it "f0 := @parameter0: float; f1 = f0 * 2; return f1; (@parameter0 = 2.0)" $ do
      let nv = [("@parameter0", LocalAddr 0),
                ("f0",          LocalAddr 1),
                ("f1",          LocalAddr 2)]
      let st = [(LocalAddr 0, VariableVal (TFloat, VFloat 2.0)),
                (LocalAddr 1, VariableVal (TFloat, defaultValue TFloat)),
                (LocalAddr 2, VariableVal (TFloat, defaultValue TFloat))]
      let stmts = [Identity "f0" (IDParameter 0) TFloat,
                   Assign (VLocal "f1") (EBinop (ILocalName "f0") Mult (IInt 2)),
                   Return (Just (ILocalName "f1"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (VFloat 4.0))

  describe "Complex statements" $ do
    it "i0 = staticinvoke <Example: int add(int, int)>(2, 4); return i0;" $ do
      let nv = [("i0", LocalAddr 0)]
      let st = [(LocalAddr 0,        VariableVal (TInt, defaultValue TInt)),
                (FileAddr "Example", FileVal exampleFile)]
      let stmts = [Assign (VLocal "i0") (EInvoke (StaticInvoke addSignature [IInt 2, IInt 4])),
                   Return (Just (ILocalName "i0"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (VInt 6))

  describe "Complete file" $ do
    it "10! = 3628800" $ do
      let st = [(FileAddr "java.lang.Object", FileVal objectFile),
                (FileAddr "java.lang.IllegalArgumentException", FileVal illegalArgumentExceptionFile),
                (FileAddr "FactorialExample", FileVal factorialExampleFile)]
      runFileConcrete env st factorialExampleFile [IInt 10] `shouldBe` Success (Just (VInt 3628800))

  where
    env = []
    store = []
    addMethodBody = MFull {
      declarations = [
        (TInt, ["i0", "i1", "i2"])
      ],
      statements = [
        Identity "i0" (IDParameter 0) TInt,
        Identity "i1" (IDParameter 1) TInt,
        Assign (VLocal "i2") (EBinop (ILocalName "i0") Plus (ILocalName "i1")),
        Return (Just (ILocalName "i2"))
      ],
      catchClauses = []
    }
    addMethod = Method { methodModifiers = [Static, Public]
                       , returnType = TInt
                       , methodName = "add"
                       , parameters = [TInt, TInt]
                       , throws = []
                       , methodBody = addMethodBody }
    exampleFile = File { fileModifiers = [Public]
                       , fileType = FTClass
                       , fileName = "Example"
                       , extends = Nothing
                       , implements = []
                       , fileBody = [MethodMember addMethod] }
    addSignature = MethodSignature "Example" TInt "add" [TInt, TInt]

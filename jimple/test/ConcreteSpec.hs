module ConcreteSpec where

import Concrete
import Syntax
import Data.Concrete.Error

import qualified Data.Map as Map

import Test.Hspec

-- import examples.SimpleExampleAbstract (file)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Literals" $ do
    it "LocalName lookup" $ do
      let expr = EImmediate (ILocalName "x")
      let nv = [(LocalPointer "x", LocalVal (TInt, Just (VInt 2)))]
      evalConcrete nv expr `shouldBe` Success (VInt 2)
    it "Integer literals" $ do
      let expr = EImmediate (IInt 7)
      evalConcrete env expr `shouldBe` Success (VInt 7)
    it "Float literals" $ do
      let expr = EImmediate (IFloat 2.5)
      evalConcrete env expr `shouldBe` Success (VFloat 2.5)
    it "String literals" $ do
      let expr = EImmediate (IString "Hello World")
      evalConcrete env expr `shouldBe` Success (VString "Hello World")
    it "Class literals" $ do
      let expr = EImmediate (IClass "java.lang.Object")
      evalConcrete env expr `shouldBe` Success (VClass "java.lang.Object")
    it "Null literals" $ do
      let expr = EImmediate (INull)
      evalConcrete env expr `shouldBe` Success (VNull)

  describe "Simple Expressions" $ do
    it "-3" $ do
      let expr = EUnop Neg (IInt 3)
      evalConcrete env expr `shouldBe` Success (VInt (-3))
    it "lengthof [1, 2, 3]" $ do
      let expr = EUnop Lengthof (ILocalName "x")
      let nv = [(LocalPointer "x", LocalVal (TArray TInt, Just (VArray [VInt 1, VInt 2, VInt 3])))]
      evalConcrete nv expr `shouldBe` Success (VInt 3)
    it "8 + 2" $ do
      let expr = EBinop (IInt 8) Plus (IInt 2)
      evalConcrete env expr `shouldBe` Success (VInt 10)
    it "8 / 0" $ do
      let expr = EBinop (IInt 8) Div (IInt 0)
      evalConcrete env expr `shouldBe` Fail "Cannot divide by zero"
    it "3 < 3.5" $ do
      let expr = EBinop (IInt 3) Cmplt (IFloat 3.5)
      evalConcrete env expr `shouldBe` Success (VBool True)
    it "3 != 'three'" $ do
      let expr = EBinop (IInt 3) Cmpne (IString "three")
      evalConcrete env expr `shouldBe` Success (VBool True)
    it "3 % 2.5" $ do
      let expr = EBinop (IInt 3) Mod (IFloat 2.5)
      evalConcrete env expr `shouldBe` Success (VFloat 0.5)
    it "new boolean" $ do
      let expr = ENew (NewSimple TBoolean)
      evalConcrete env expr `shouldBe` Success (VInt 0)
    it "[1, 2, 3][2]" $ do
      let expr = EReference (ArrayReference "xs" (IInt 2))
      let nv = [(LocalPointer "xs", LocalVal (TArray TInt, Just (VArray [VInt 1, VInt 2, VInt 3])))]
      evalConcrete nv expr `shouldBe` Success (VInt 3)
    it "p.<Person: int age>" $ do
      let personAgeSignature = FieldSignature "Person" TInt "age"
      let personObject = Map.fromList [("age", VInt 10)]
      let nv = [(LocalPointer "p", LocalVal (TClass "Person", Just (VObject "Person" personObject)))]
      let expr = EReference (FieldReference "p" personAgeSignature)
      evalConcrete nv expr `shouldBe` Success (VInt 10)
    it "<Person: int MAX_AGE>" $ do
      let maxAgeSignature = FieldSignature "Person" TInt "MAX_AGE"
      let nv = [(FieldPointer maxAgeSignature, FieldVal (Just (VInt 100)))]
      let expr = EReference (SignatureReference maxAgeSignature)
      evalConcrete nv expr `shouldBe` Success (VInt 100)
    it "newmultiarray (float) [3][]" $ do
      let expr = ENew (NewMulti TFloat [IInt 3, IInt 2])
      evalConcrete env expr `shouldBe` Success (VArray [VArray [VFloat 0.0, VFloat 0.0],
                                                 VArray [VFloat 0.0, VFloat 0.0],
                                                 VArray [VFloat 0.0, VFloat 0.0]])

  describe "Simple Statements" $ do
    it "i0 = 2 + 3;" $ do
      let stmts = [Assign (VLocal "i0") (EBinop (IInt 2) Plus (IInt 3)),
                   Return (Just (ILocalName "i0"))]
      let nv = [(LocalPointer "i0", LocalVal (TInt, Nothing))]
      runStatementsConcrete nv stmts `shouldBe` Success (Just (VInt 5))
    -- it "xs = newarray (int)[s]; (s = 2)" $ do
    --   let stmts = [Assign (VLocal "xs") (ENew (NewArray TInt (ILocalName "s")))]
    --   let nv = Map.fromList [("s", "p0"), ("xs", "p1")]
    --   let st1 = (Map.empty, Map.empty, Map.fromList [("p0", (TInt, Just (VInt 2))),
    --                                                  ("p1", (TArray TInt, Nothing))])
    --   let st2 = (Map.empty, Map.empty, Map.fromList [("p0", (TInt, Just (VInt 2))),
    --                                                  ("p1", (TArray TInt, Just (VArray [VInt 0, VInt 0])))])
    --   runStatements' nv st1 stmts `shouldBe` Success (nv, st2, Nothing)
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let stmts = [If (EBinop (IInt 2) Cmple (IInt 3)) "l2",
                   Label "l1",
                   Return (Just (IInt 1)),
                   Label "l2",
                   Return (Just (IInt 0))]
      runStatementsConcrete env stmts `shouldBe` Success (Just (VInt 0))
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
      runStatementsConcrete env stmts `shouldBe` Success (Just (VInt 2))
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
      runStatementsConcrete env stmts `shouldBe` Success (Just (VInt 3))

    -- describe "Complete file" $ do
    --   run' store file `shouldBe` Right (store, Nothing)

  where
    env = []

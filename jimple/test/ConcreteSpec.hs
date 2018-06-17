module ConcreteSpec where

import Test.Hspec

import Utils

import Syntax
import ConcreteSemantics

import           Data.Exception
import qualified Data.Map as Map
import           Data.Concrete.Error

import Classes.Throwable
import Classes.ArrayFieldExample
import Classes.FactorialExample
import Classes.PersonExample
import Classes.SingleMethodExample
import Classes.TryCatchExample

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Literals" $ do
    it "LocalName lookup" $ do
      let i = Local "x"
      let mem = [("x", IntVal 2)]
      evalImmediate' mem i `shouldBe` Success (IntVal 2)
    it "Integer literals" $ do
      let i = IntConstant 7
      evalImmediate' [] i `shouldBe` Success (IntVal 7)
    it "Float literals" $ do
      let i = FloatConstant 2.5
      evalImmediate' [] i `shouldBe` Success (FloatVal 2.5)
    it "String literals" $ do
      let i = StringConstant "Hello World"
      evalImmediate' [] i `shouldBe` Success (StringVal "Hello World")
    it "Class literals" $ do
      let i = ClassConstant "java.lang.Object"
      evalImmediate' [] i `shouldBe` Success (ClassVal "java.lang.Object")
    it "Null literals" $ do
      let i = NullConstant
      evalImmediate' [] i `shouldBe` Success NullVal

  describe "Boolean expressions" $ do
    it "3 < 4" $ do
      let expr = BoolExpr (IntConstant 3) Cmplt (IntConstant 4)
      evalBool' [] expr `shouldBe` Success True
    it "3 != 'three'" $ do
      let expr = BoolExpr (IntConstant 3) Cmpne (StringConstant "three")
      evalBool' [] expr `shouldBe` Success True

  describe "Simple Expressions" $ do
    it "-3" $ do
      let expr = UnopExpr Neg (IntConstant 3)
      eval' [] expr `shouldBe` Success (IntVal (-3))
    it "lengthof [1, 2, 3]" $ do
      let expr = UnopExpr Lengthof (Local "x")
      let mem = [("x",ArrayVal [IntVal 1, IntVal 2, IntVal 3])]
      eval' mem expr `shouldBe` Success (IntVal 3)
    it "8 + 2" $ do
      let expr = BinopExpr (IntConstant 8) Plus (IntConstant 2)
      eval' [] expr `shouldBe` Success (IntVal 10)
    it "8 / 0" $ do
      let expr = BinopExpr (IntConstant 8) Div (IntConstant 0)
      eval' [] expr `shouldBe` staticException "CompilationUnit \"java.lang.ArithmeticException\" not loaded"
    it "3.0 % 2.5" $ do
      let expr = BinopExpr (FloatConstant 3.0) Rem (FloatConstant 2.5)
      eval' [] expr `shouldBe` Success (FloatVal 0.5)
    it "new boolean" $ do
      let expr = NewExpr BooleanType
      eval' [] expr `shouldBe` Success (IntVal 0)
    it "[1, 2, 3][2]" $ do
      let expr = RefExpr (ArrayRef "xs" (IntConstant 2))
      let mem = [("xs",ArrayVal [IntVal 1, IntVal 2, IntVal 3])]
      eval' mem expr `shouldBe` Success (IntVal 3)
    it "newmultiarray (float) [3][2]" $ do
      let expr = NewMultiArrayExpr FloatType [IntConstant 3, IntConstant 2]
      eval' [] expr `shouldBe` Success (ArrayVal [ArrayVal [FloatVal 0.0, FloatVal 0.0],
                                                 ArrayVal [FloatVal 0.0, FloatVal 0.0],
                                                 ArrayVal [FloatVal 0.0, FloatVal 0.0]])

  describe "Simple Statements" $ do
    it "i0 = 2 + 3; return i0;" $ do
      let mem = [("i0",IntVal 0)]
      let stmts = [Assign (LocalVar "i0") (BinopExpr (IntConstant 2) Plus (IntConstant 3)),
                   Return (Just (Local "i0"))]
      runStatements' mem stmts `shouldBe` Success (Just (IntVal 5))
    it "assign non-declared variable" $ do
      let stmts = [Assign (LocalVar "s") (ImmediateExpr (IntConstant 2))]
      runStatements' [] stmts `shouldBe` staticException "Variable \"s\" not bound"
    it "s = 2; xs = newarray (int)[s]; y = lengthof xs; return xs;" $ do
      let mem = [("s",  IntVal 0),
                  ("xs", NullVal),
                  ("y",  IntVal 0)]
      let stmts = [Assign (LocalVar "s") (ImmediateExpr (IntConstant 2)),
                   Assign (LocalVar "xs") (NewArrayExpr IntType (Local "s")),
                   Assign (LocalVar "y") (UnopExpr Lengthof (Local "xs")),
                   Return (Just (Local "y"))]
      runStatements' mem stmts `shouldBe` Success (Just (IntVal 2))
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let stmts = [If (BoolExpr (IntConstant 2) Cmple (IntConstant 3)) "l2",
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 0))]
      runStatements' [] stmts `shouldBe` Success (Just (IntVal 0))
    it "lookupswitch(4) { case 0: goto l1; case 4: goto l2; default: goto l3;}; l1: return 1; l2: return 2; l3: return 3;" $ do
      let stmts = [Lookupswitch (IntConstant 4) [(ConstantCase 0, "l1"),
                                                 (ConstantCase 4, "l2"),
                                                 (DefaultCase, "l3")],
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 2)),
                   Label "l3",
                   Return (Just (IntConstant 3))]
      runStatements' [] stmts `shouldBe` Success (Just (IntVal 2))
    it "lookupswitch(2) { case 0: goto l1; case 4: goto l2; default: goto l3;}; l1: return 1; l2: return 2; l3: return 3;" $ do
      let stmts = [Lookupswitch (IntConstant 2) [(ConstantCase 0, "l1"),
                                          (ConstantCase 4, "l2"),
                                          (DefaultCase, "l3")],
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 2)),
                   Label "l3",
                   Return (Just (IntConstant 3))]
      runStatements' [] stmts `shouldBe` Success (Just (IntVal 3))
    it "f0 := @parameter0: float; f1 = f0 * 2.5; return f1; (@parameter0 = 2.0)" $ do
      let mem = [("@parameter0", FloatVal 2.0),
                 ("f0",          FloatVal 0.0),
                 ("f1",          FloatVal 0.0)]
      let stmts = [Identity "f0" (ParameterRef 0) FloatType,
                   Assign (LocalVar "f1") (BinopExpr (Local "f0") Mult (FloatConstant 2.5)),
                   Return (Just (Local "f1"))]
      runStatements' mem stmts `shouldBe` Success (Just (FloatVal 5.0))

  describe "Complete program" $ do
    it "10! = 3628800" $
      runProgram'' factorialExampleFile [IntConstant 10] `shouldBe` Success (Just (IntVal 3628800))
    it "s = new SingleMethodExample; s.x = 2; return s.x" $
      runProgram'' singleMethodExampleFile [] `shouldBe` Success (Just (IntVal 2))
    it "(-10)! throws IllegalArgumentException" $
      runProgram'' factorialExampleFile [IntConstant (-10)] `shouldBe` dynamicException "java.lang.IllegalArgumentException" "Negative value for argument n"
    it "5 -> [5,10,5,10]" $
      runProgram'' arrayFieldExampleFile [IntConstant 5] `shouldBe` Success (Just (ArrayVal [IntVal 5, IntVal 10, IntVal 5, IntVal 10]))
    it "(new Person(10)).yearsToLive() = 90" $
      runProgram'' personExampleFile [] `shouldBe` Success (Just (IntVal 90))
    it "try { throw e } catch (e) { throw e' }" $
      runProgram'' tryCatchExampleFile [] `shouldBe` dynamicException "java.lang.ArrayIndexOutOfBoundsException" "b"

  where
    staticException msg = Fail (StaticException msg)
    dynamicException clzz msg = Fail (DynamicException (ObjectVal clzz (Map.fromList [(throwableMessageSignature, StringVal msg)])))
    runProgram'' unit params = runProgram' (unit:baseCompilationUnits) (mainMethod unit,params)

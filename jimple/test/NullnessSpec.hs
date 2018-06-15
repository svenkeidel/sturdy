{-# LANGUAGE Arrows #-}
module NullnessSpec where

import           Prelude hiding (Bool(..))
import qualified Prelude as P

import Test.Hspec

import Utils

import Syntax
import Shared
import Nullness

import           Data.Exception
import           Data.List
import           Data.Abstract.Boolean as B
import           Data.Abstract.HandleError

import Control.Arrow

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
      let expr = Local "x"
      let mem = [("x",NonNull)]
      evalImmediate_ mem expr `shouldSatisfy` ifSuccess NonNull
    it "Integer literals" $ do
      let expr = IntConstant 7
      evalImmediate_ [] expr `shouldBe` Success NonNull
    it "Float literals" $ do
      let expr = FloatConstant 2.5
      evalImmediate_ [] expr `shouldBe` Success NonNull
    it "String literals" $ do
      let expr = StringConstant "Hello World"
      evalImmediate_ [] expr `shouldBe` Success NonNull
    it "Class literals" $ do
      let expr = ClassConstant "java.lang.Object"
      evalImmediate_ [] expr `shouldBe` Success NonNull
    it "Null literals" $ do
      let expr = NullConstant
      evalImmediate_ [] expr `shouldBe` Success Null

  describe "Boolean expressions" $ do
    it "3 < 4" $ do
      let expr = BoolExpr (IntConstant 3) Cmplt (IntConstant 4)
      evalBool_ [] expr `shouldBe` Success B.Top
    it "3 != 'three'" $ do
      let expr = BoolExpr (IntConstant 3) Cmpne (StringConstant "three")
      evalBool_ [] expr `shouldBe` Success B.Top

  describe "Simple Expressions" $ do
    it "-3" $ do
      let expr = UnopExpr Neg (IntConstant 3)
      eval_ [] expr `shouldBe` Success NonNull
    it "lengthof [1, 2, 3]" $ do
      let expr = UnopExpr Lengthof (Local "x")
      let mem = [("x",NonNull)]
      eval_ mem expr `shouldSatisfy` ifSuccess NonNull
    it "8 + 2" $ do
      let expr = BinopExpr (IntConstant 8) Plus (IntConstant 2)
      eval_ [] expr `shouldBe` Success NonNull
    it "8 / 0" $ do
      let expr = BinopExpr (IntConstant 8) Div (IntConstant 0)
      eval_ [] expr `shouldBe` SuccessOrFail (DynamicException NonNull) NonNull
    it "3.0 % 2.5" $ do
      let expr = BinopExpr (FloatConstant 3.0) Rem (FloatConstant 2.5)
      eval_ [] expr `shouldBe` Success NonNull
    it "new boolean" $ do
      let expr = NewExpr BooleanType
      eval_ [] expr `shouldBe` Success NonNull
    it "[1, 2, 3][2]" $ do
      let expr = RefExpr (ArrayRef "xs" (IntConstant 2))
      let mem = [("xs",NonNull)]
      eval_ mem expr `shouldSatisfy` ifSuccess NonNull
    it "newmultiarray (float) [3][2]" $ do
      let expr = NewMultiArrayExpr FloatType [IntConstant 3, IntConstant 2]
      eval_ [] expr `shouldSatisfy` ifSuccess NonNull

  describe "Simple Statements" $ do
    it "i0 = 2 + 3; return i0;" $ do
      let mem = [("i0",NonNull)]
      let stmts = [Assign (LocalVar "i0") (BinopExpr (IntConstant 2) Plus (IntConstant 3)),
                   Return (Just (Local "i0"))]
      runStatements_ mem stmts `shouldSatisfy` ifSuccess (Just NonNull)
    it "assign non-declared variable" $ do
      let stmts = [Assign (LocalVar "s") (ImmediateExpr (IntConstant 2))]
      runStatements_ [] stmts `shouldBe` staticException "Variable \"s\" not bound"
    it "s = 2; xs = newarray (int)[s]; y = lengthof xs; return xs;" $ do
      let mem = [("s",  NonNull),
                ("xs", Null),
                ("y",  NonNull)]
      let stmts = [Assign (LocalVar "s") (ImmediateExpr (IntConstant 2)),
                   Assign (LocalVar "xs") (NewArrayExpr IntType (Local "s")),
                   Assign (LocalVar "y") (UnopExpr Lengthof (Local "xs")),
                   Return (Just (Local "y"))]
      runStatements_ mem stmts `shouldSatisfy` ifSuccess (Just NonNull)
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let stmts = [If (BoolExpr (IntConstant 2) Cmple (IntConstant 3)) "l2",
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 0))]
      runStatements_ [] stmts `shouldBe` Success (Just NonNull)
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
      runStatements_ [] stmts `shouldSatisfy` ifSuccess (Just NonNull)
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
      runStatements_ [] stmts `shouldSatisfy` ifSuccess (Just NonNull)
    it "f0 := @parameter0: float; f1 = f0 * 2.5; return f1; (@parameter0 = 2.0)" $ do
      let mem = [("@parameter0", NonNull),
                ("f0",          NonNull),
                ("f1",          NonNull)]
      let stmts = [Identity "f0" (ParameterRef 0) FloatType,
                   Assign (LocalVar "f1") (BinopExpr (Local "f0") Mult (FloatConstant 2.5)),
                   Return (Just (Local "f1"))]
      runStatements_ mem stmts `shouldSatisfy` ifSuccess (Just NonNull)
  --
  -- describe "Complete program" $ do
  --   it "10! = 3628800" $ do
  --     let files = baseCompilationUnits ++ [factorialExampleFile]
  --     runProgram_ files factorialExampleFile [IntConstant 10] `shouldSatisfy` ifSuccess (Just NonNull)
  --   it "s = new SingleMethodExample; s.x = 2; return s.x" $ do
  --     let files = baseCompilationUnits ++ [singleMethodExampleFile]
  --     runProgram_ files singleMethodExampleFile [] `shouldSatisfy` ifSuccess (Just NonNull)
  --   it "(-10)! throws IllegalArgumentException" $ do
  --     let files = baseCompilationUnits ++ [factorialExampleFile]
  --     runProgram_ files factorialExampleFile [IntConstant (-10)] `shouldBe` dynamicException "java.lang.IllegalArgumentException"
  --   it "5 -> [5,10,5,10]" $ do
  --     let files = baseCompilationUnits ++ [arrayFieldExampleFile]
  --     runProgram_ files arrayFieldExampleFile [IntConstant 5] `shouldSatisfy` ifSuccess (Just NonNull)
  --   it "(new Person(10)).yearsToLive() = 90" $ do
  --     let files = baseCompilationUnits ++ [personExampleFile]
  --     runProgram_ files personExampleFile [] `shouldSatisfy` ifSuccess (Just NonNull)
  --   it "try { throw e } catch (e) { throw e' }" $ do
  --     let files = baseCompilationUnits ++ [tryCatchExampleFile]
  --     runProgram_ files tryCatchExampleFile [] `shouldBe` dynamicException "java.lang.ArrayIndexOutOfBoundsException"

  where
    staticException msg = Fail (StaticException msg)
    dynamicException clzz = Fail (DynamicException NonNull)

    ifSuccess x e = case e of
      Success y -> y == x
      SuccessOrFail _ y -> y == x
      _ -> P.False

    evalImmediate_ = runInterp evalImmediate (testCompilationUnits []) (testMethod [])
    evalBool_ = runInterp evalBool (testCompilationUnits []) (testMethod [])
    eval_ = runInterp eval (testCompilationUnits []) (testMethod [])

    runStatements_ mem stmts =
      runInterp runStatements (testCompilationUnits stmts) (testMethod stmts) mem stmts

    runProgram_ compilationUnits mainUnit =
      runInterp runProgram compilationUnits (mainMethod mainUnit) []

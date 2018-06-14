{-# LANGUAGE Arrows #-}
module NullnessSpec where

import           Prelude hiding (Bool(..))
import qualified Prelude as P

import Syntax
import Shared
import Nullness

import           Data.List
import           Data.Abstract.Boolean
import           Data.Abstract.HandleError

import Control.Arrow

import Test.Hspec

import Classes.Object
import Classes.Throwable
import Classes.IllegalArgumentException
import Classes.ArrayIndexOutOfBoundsException
import Classes.ArithmeticException

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
      let nv = [("x", 1)]
      let st = [(1, NonNull)]
      evalImmediate_ nv st expr `shouldSatisfy` ifSuccess NonNull
    it "Integer literals" $ do
      let expr = IntConstant 7
      evalImmediate_ env store expr `shouldBe` Success NonNull
    it "Float literals" $ do
      let expr = FloatConstant 2.5
      evalImmediate_ env store expr `shouldBe` Success NonNull
    it "String literals" $ do
      let expr = StringConstant "Hello World"
      evalImmediate_ env store expr `shouldBe` Success NonNull
    it "Class literals" $ do
      let expr = ClassConstant "java.lang.Object"
      evalImmediate_ env store expr `shouldBe` Success NonNull
    it "Null literals" $ do
      let expr = NullConstant
      evalImmediate_ env store expr `shouldBe` Success Null

  describe "Boolean expressions" $ do
    it "3 < 4" $ do
      let expr = BoolExpr (IntConstant 3) Cmplt (IntConstant 4)
      evalBool_ env store expr `shouldBe` Success True
    it "3 != 'three'" $ do
      let expr = BoolExpr (IntConstant 3) Cmpne (StringConstant "three")
      evalBool_ env store expr `shouldBe` Success True

  describe "Simple Expressions" $ do
    it "-3" $ do
      let expr = UnopExpr Neg (IntConstant 3)
      eval_ env store expr `shouldBe` Success NonNull
    it "lengthof [1, 2, 3]" $ do
      let expr = UnopExpr Lengthof (Local "x")
      let nv = [("x", 1)]
      let st = [(1, NonNull),
                (2, NonNull)]
      eval_ nv st expr `shouldSatisfy` ifSuccess NonNull
    it "8 + 2" $ do
      let expr = BinopExpr (IntConstant 8) Plus (IntConstant 2)
      eval_ env store expr `shouldBe` Success NonNull
    it "8 / 0" $ do
      let expr = BinopExpr (IntConstant 8) Div (IntConstant 0)
      eval_ env store expr `shouldBe` Fail (DynamicException NonNull)
    it "3.0 % 2.5" $ do
      let expr = BinopExpr (FloatConstant 3.0) Rem (FloatConstant 2.5)
      eval_ env store expr `shouldBe` Success NonNull
    it "new boolean" $ do
      let expr = NewExpr BooleanType
      eval_ env store expr `shouldBe` Success NonNull
    it "[1, 2, 3][2]" $ do
      let expr = RefExpr (ArrayRef "xs" (IntConstant 2))
      let nv = [("xs", 1)]
      let st = [(1, NonNull),
                (2, NonNull)]
      eval_ nv st expr `shouldSatisfy` ifSuccess NonNull
    it "newmultiarray (float) [3][2]" $ do
      let expr = NewMultiArrayExpr FloatType [IntConstant 3, IntConstant 2]
      eval_ env store expr `shouldSatisfy` ifSuccess NonNull

  describe "Simple Statements" $ do
    it "i0 = 2 + 3; return i0;" $ do
      let nv = [("i0", 1)]
      let st = [(1, NonNull)]
      let stmts = [Assign (LocalVar "i0") (BinopExpr (IntConstant 2) Plus (IntConstant 3)),
                   Return (Just (Local "i0"))]
      runStatements_ nv st stmts `shouldSatisfy` ifSuccess (Just NonNull)
    it "assign non-declared variable" $ do
      let stmts = [Assign (LocalVar "s") (ImmediateExpr (IntConstant 2))]
      runStatements_ env store stmts `shouldBe` staticException "Variable \"s\" not bound"
    it "s = 2; xs = newarray (int)[s]; y = lengthof xs; return xs;" $ do
      let nv = [("s", 0),
                ("xs", 1),
                ("y", 2)]
      let st = [(0, NonNull),
                (1, Null),
                (2, NonNull)]
      let stmts = [Assign (LocalVar "s") (ImmediateExpr (IntConstant 2)),
                   Assign (LocalVar "xs") (NewArrayExpr IntType (Local "s")),
                   Assign (LocalVar "y") (UnopExpr Lengthof (Local "xs")),
                   Return (Just (Local "y"))]
      runStatements_ nv st stmts `shouldSatisfy` ifSuccess (Just NonNull)
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let stmts = [If (BoolExpr (IntConstant 2) Cmple (IntConstant 3)) "l2",
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 0))]
      runStatements_ env store stmts `shouldBe` Success (Just NonNull)
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
      runStatements_ env store stmts `shouldSatisfy` ifSuccess (Just NonNull)
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
      runStatements_ env store stmts `shouldSatisfy` ifSuccess (Just NonNull)
    it "f0 := @parameter0: float; f1 = f0 * 2.5; return f1; (@parameter0 = 2.0)" $ do
      let nv = [("@parameter0", 0),
                ("f0",          1),
                ("f1",          2)]
      let st = [(0, NonNull),
                (1, NonNull),
                (2, NonNull)]
      let stmts = [Identity "f0" (ParameterRef 0) FloatType,
                   Assign (LocalVar "f1") (BinopExpr (Local "f0") Mult (FloatConstant 2.5)),
                   Return (Just (Local "f1"))]
      runStatements_ nv st stmts `shouldSatisfy` ifSuccess (Just NonNull)

  describe "Complete program" $ do
    it "10! = 3628800" $ do
      let files = baseCompilationUnits ++ [factorialExampleFile]
      runProgram_ files factorialExampleFile [IntConstant 10] `shouldSatisfy` ifSuccess (Just NonNull)
    it "s = new SingleMethodExample; s.x = 2; return s.x" $ do
      let files = baseCompilationUnits ++ [singleMethodExampleFile]
      runProgram_ files singleMethodExampleFile [] `shouldSatisfy` ifSuccess (Just NonNull)
    it "(-10)! throws IllegalArgumentException" $ do
      let files = baseCompilationUnits ++ [factorialExampleFile]
      runProgram_ files factorialExampleFile [IntConstant (-10)] `shouldBe` dynamicException "java.lang.IllegalArgumentException"
    it "5 -> [5,10,5,10]" $ do
      let files = baseCompilationUnits ++ [arrayFieldExampleFile]
      runProgram_ files arrayFieldExampleFile [IntConstant 5] `shouldSatisfy` ifSuccess (Just NonNull)
    it "(new Person(10)).yearsToLive() = 90" $ do
      let files = baseCompilationUnits ++ [personExampleFile]
      runProgram_ files personExampleFile [] `shouldSatisfy` ifSuccess (Just NonNull)
    it "try { throw e } catch (e) { throw e' }" $ do
      let files = baseCompilationUnits ++ [tryCatchExampleFile]
      runProgram_ files tryCatchExampleFile [] `shouldBe` dynamicException "java.lang.ArrayIndexOutOfBoundsException"

  where
    baseCompilationUnits = [objectFile,
                            throwableFile,
                            illegalArgumentExceptionFile,
                            arrayIndexOutOfBoundsExceptionFile,
                            arithmeticExceptionFile]
    env = []
    store = []

    staticException msg = Fail (StaticException msg)
    dynamicException clzz = Fail (DynamicException NonNull)

    ifSuccess x e = case e of
      Success y -> y == x
      SuccessOrFail _ y -> y == x
      _ -> P.False

    testMethodBody stmts = FullBody { declarations = []
                                 , statements = stmts
                                 , catchClauses = []
                                 }

    testMethod stmts = Method { methodModifiers = [Public, Static]
                              , returnType = VoidType
                              , methodName = "test"
                              , parameters = []
                              , throws = []
                              , methodBody = testMethodBody stmts
                              }

    testCompilationUnits stmts = CompilationUnit { fileModifiers = [Public]
                                                 , fileType = ClassFile
                                                 , fileName = "Test"
                                                 , extends = Just "java.lang.Object"
                                                 , implements = []
                                                 , fileBody = [MethodMember (testMethod stmts)]
                                                 } : baseCompilationUnits

    mainMethod unit =
      case find (\m -> methodName m == "main") [m | MethodMember m <- fileBody unit] of
        Just m -> m
        Nothing -> error "No entry method found"

    deepDerefMaybe = proc val -> case val of
      Just x -> deepDeref >>^ Just -< x
      Nothing -> returnA -< Nothing

    evalImmediate_ env' store' = runInterp evalImmediate (testCompilationUnits []) env' store' (testMethod [])
    evalBool_ env' store' = runInterp evalBool (testCompilationUnits []) env' store' (testMethod [])
    eval_ env' store' = runInterp (eval >>> deepDeref) (testCompilationUnits []) env' store' (testMethod [])

    runStatements_ env' store' stmts =
      runInterp (runStatements >>> deepDerefMaybe) (testCompilationUnits stmts) env' store' (testMethod stmts) stmts

    runProgram_ compilationUnits mainUnit =
      runInterp (runProgram >>> deepDerefMaybe) compilationUnits [] [] (mainMethod mainUnit)

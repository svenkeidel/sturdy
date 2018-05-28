module ConcreteSpec where

import Syntax
import Shared
import Concrete

import qualified Data.Map as Map
import Data.List
import Data.Concrete.Error
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
      let st = [(1, IntVal 2)]
      evalConcrete nv st expr `shouldBe` Success (IntVal 2)
    it "Integer literals" $ do
      let expr = IntConstant 7
      evalConcrete env store expr `shouldBe` Success (IntVal 7)
    it "Float literals" $ do
      let expr = FloatConstant 2.5
      evalConcrete env store expr `shouldBe` Success (FloatVal 2.5)
    it "String literals" $ do
      let expr = StringConstant "Hello World"
      evalConcrete env store expr `shouldBe` Success (StringVal "Hello World")
    it "Class literals" $ do
      let expr = ClassConstant "java.lang.Object"
      evalConcrete env store expr `shouldBe` Success (ClassVal "java.lang.Object")
    it "Null literals" $ do
      let expr = NullConstant
      evalConcrete env store expr `shouldBe` Success NullVal

  describe "Simple Expressions" $ do
    it "-3" $ do
      let expr = UnopExpr Neg (IntConstant 3)
      evalConcrete env store expr `shouldBe` Success (IntVal (-3))
    it "lengthof [1, 2, 3]" $ do
      let expr = UnopExpr Lengthof (Local "x")
      let nv = [("x", 1)]
      let st = [(1, RefVal 2),
                (2, ArrayVal [IntVal 1, IntVal 2, IntVal 3])]
      evalConcrete nv st expr `shouldBe` Success (IntVal 3)
    it "8 + 2" $ do
      let expr = BinopExpr (IntConstant 8) Plus (IntConstant 2)
      evalConcrete env store expr `shouldBe` Success (IntVal 10)
    it "8 / 0" $ do
      let expr = BinopExpr (IntConstant 8) Div (IntConstant 0)
      evalConcrete env store expr `shouldBe` Fail (DynamicException (RefVal 0))
    it "3 < 4" $ do
      let expr = BinopExpr (IntConstant 3) Cmplt (IntConstant 4)
      evalConcrete env store expr `shouldBe` Success (BoolVal True)
    it "3 != 'three'" $ do
      let expr = BinopExpr (IntConstant 3) Cmpne (StringConstant "three")
      evalConcrete env store expr `shouldBe` Success (BoolVal True)
    it "3.0 % 2.5" $ do
      let expr = BinopExpr (FloatConstant 3.0) Mod (FloatConstant 2.5)
      evalConcrete env store expr `shouldBe` Success (FloatVal 0.5)
    it "new boolean" $ do
      let expr = NewExpr BooleanType
      evalConcrete env store expr `shouldBe` Success (BoolVal False)
    it "[1, 2, 3][2]" $ do
      let expr = ArrayRef "xs" (IntConstant 2)
      let nv = [("xs", 1)]
      let st = [(1, RefVal 2),
                (2, ArrayVal [IntVal 1, IntVal 2, IntVal 3])]
      evalConcrete nv st expr `shouldBe` Success (IntVal 3)
    it "newmultiarray (float) [3][]" $ do
      let expr = NewMultiArrayExpr FloatType [IntConstant 3, IntConstant 2]
      evalConcrete env store expr `shouldBe` Success (ArrayVal [ArrayVal [FloatVal 0.0, FloatVal 0.0],
                                                 ArrayVal [FloatVal 0.0, FloatVal 0.0],
                                                 ArrayVal [FloatVal 0.0, FloatVal 0.0]])

  describe "Simple Statements" $ do
    it "i0 = 2 + 3; return i0;" $ do
      let nv = [("i0", 1)]
      let st = [(1, IntVal 0)]
      let stmts = [Assign (LocalVar "i0") (BinopExpr (IntConstant 2) Plus (IntConstant 3)),
                   Return (Just (Local "i0"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (IntVal 5))
    it "assign non-declared variable" $ do
      let stmts = [Assign (LocalVar "s") (IntConstant 2)]
      runStatementsConcrete env store stmts `shouldBe` staticException "Variable \"s\" not bounded"
    it "s = 2; xs = newarray (int)[s]; y = lengthof xs; return xs;" $ do
      let nv = [("s", 0),
                ("xs", 1),
                ("y", 2)]
      let st = [(0, IntVal 0),
                (1, NullVal),
                (2, IntVal 0)]
      let stmts = [Assign (LocalVar "s") (IntConstant 2),
                   Assign (LocalVar "xs") (NewArrayExpr IntType (Local "s")),
                   Assign (LocalVar "y") (UnopExpr Lengthof (Local "xs")),
                   Return (Just (Local "y"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (IntVal 2))
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let stmts = [If (BinopExpr (IntConstant 2) Cmple (IntConstant 3)) "l2",
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 0))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (IntVal 0))
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
      runStatementsConcrete env store stmts `shouldBe` Success (Just (IntVal 2))
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
      runStatementsConcrete env store stmts `shouldBe` Success (Just (IntVal 3))
    it "f0 := @parameter0: float; f1 = f0 * 2.5; return f1; (@parameter0 = 2.0)" $ do
      let nv = [("@parameter0", 0),
                ("f0",          1),
                ("f1",          2)]
      let st = [(0, FloatVal 2.0),
                (1, FloatVal 0.0),
                (2, FloatVal 0.0)]
      let stmts = [Identity "f0" (ParameterRef 0) FloatType,
                   Assign (LocalVar "f1") (BinopExpr (Local "f0") Mult (FloatConstant 2.5)),
                   Return (Just (Local "f1"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (FloatVal 5.0))

  describe "Complete program" $ do
    it "10! = 3628800" $ do
      let files = baseCompilationUnits ++ [factorialExampleFile]
      runProgramConcrete files factorialExampleFile [IntConstant 10] `shouldBe` Success (Just (IntVal 3628800))
    it "s = new SingleMethodExample; s.x = 2; return s.x" $ do
      let files = baseCompilationUnits ++ [singleMethodExampleFile]
      runProgramConcrete files singleMethodExampleFile [] `shouldBe` Success (Just (IntVal 2))
    it "(-10)! throws IllegalArgumentException" $ do
      let files = baseCompilationUnits ++ [factorialExampleFile]
      runProgramConcrete files factorialExampleFile [IntConstant (-10)] `shouldBe` dynamicException "java.lang.IllegalArgumentException" "Negative value for argument n"
    it "5 -> [5, 5, 5, 5]" $ do
      let files = baseCompilationUnits ++ [arrayFieldExampleFile]
      runProgramConcrete files arrayFieldExampleFile [IntConstant 5] `shouldBe` Success (Just (ArrayVal [IntVal 5, IntVal 5, IntVal 5, IntVal 5]))
    it "(new Person(10)).yearsToLive() = 90" $ do
      let files = baseCompilationUnits ++ [personExampleFile]
      runProgramConcrete files personExampleFile [] `shouldBe` Success (Just (IntVal 90))
    it "try { throw e } catch (e) { throw e' }" $ do
      let files = baseCompilationUnits ++ [tryCatchExampleFile]
      runProgramConcrete files tryCatchExampleFile [] `shouldBe` dynamicException "java.lang.ArrayIndexOutOfBoundsException" "b"

  where
    baseCompilationUnits = [objectFile,
                            throwableFile,
                            illegalArgumentExceptionFile,
                            arrayIndexOutOfBoundsExceptionFile,
                            arithmeticExceptionFile]
    env = []
    store = []

    staticException msg = Fail (StaticException msg)
    dynamicException clzz msg = Fail (DynamicException (ObjectVal clzz (Map.fromList [(throwableMessageSignature, StringVal msg)])))

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

    testCompilationUnits stmts = [CompilationUnit { fileModifiers = [Public]
                                                  , fileType = ClassFile
                                                  , fileName = "Test"
                                                  , extends = Just "java.lang.Object"
                                                  , implements = []
                                                  , fileBody = [MethodMember (testMethod stmts)]
                                                  }] ++ baseCompilationUnits
    getMethod :: Member -> [Method]
    getMethod (MethodMember m) = [m]
    getMethod _ = []
    getMethods members = concatMap getMethod members
    mainMethod unit = case find (\m -> methodName m == "main") (getMethods (fileBody unit)) of
      Just m -> m
      Nothing -> error "No entry method found"

    evalConcrete env' store' = runInterp (eval >>> unbox) (testCompilationUnits []) env' store' (testMethod [])

    runStatementsConcrete env' store' stmts =
      runInterp (runStatements >>> unboxMaybe) (testCompilationUnits stmts) env' store' (testMethod stmts) stmts

    runProgramConcrete compilationUnits mainUnit args =
      runInterp (runProgram >>> unboxMaybe) compilationUnits [] [] (mainMethod mainUnit) args

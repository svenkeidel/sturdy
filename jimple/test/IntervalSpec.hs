{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImplicitParams #-}
module IntervalSpec where

import Prelude hiding (Bool(..))

import Syntax
import Shared
import Interval

import qualified Data.Map as Map
import           Data.List
import           Data.Boolean
import           Data.Abstract.Bounded
import           Data.Abstract.Error
import qualified Data.Abstract.Interval as I

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
      let ?bound = I.Interval (-100) 100
      let expr = Local "x"
      let nv = [("x", 1)]
      let st = [(1, int 2 2)]
      evalConcrete nv st expr `shouldBe` Success (int 2 2)
    it "Integer literals" $ do
      let ?bound = I.Interval (-100) 100
      let expr = IntConstant 7
      evalConcrete env store expr `shouldBe` Success (int 7 7)
    it "Float literals" $ do
      let ?bound = I.Interval (-100) 100
      let expr = FloatConstant 2.5
      evalConcrete env store expr `shouldBe` Success (FloatVal 2.5)
    it "String literals" $ do
      let ?bound = I.Interval (-100) 100
      let expr = StringConstant "Hello World"
      evalConcrete env store expr `shouldBe` Success (StringVal "Hello World")
    it "Class literals" $ do
      let ?bound = I.Interval (-100) 100
      let expr = ClassConstant "java.lang.Object"
      evalConcrete env store expr `shouldBe` Success (ClassVal "java.lang.Object")
    it "Null literals" $ do
      let ?bound = I.Interval (-100) 100
      let expr = NullConstant
      evalConcrete env store expr `shouldBe` Success NullVal

  describe "Simple Expressions" $ do
    it "-3" $ do
      let ?bound = I.Interval (-100) 100
      let expr = UnopExpr Neg (IntConstant 3)
      evalConcrete env store expr `shouldBe` Success (int (-3) (-3))
    it "lengthof [1, 2, 3]" $ do
      let ?bound = I.Interval (-100) 100
      let expr = UnopExpr Lengthof (Local "x")
      let nv = [("x", 1)]
      let st = [(1, RefVal 2),
                (2, ArrayVal (int 1 3) (int 3 3))]
      evalConcrete nv st expr `shouldBe` Success (int 3 3)
    it "8 + 2" $ do
      let ?bound = I.Interval (-100) 100
      let expr = BinopExpr (IntConstant 8) Plus (IntConstant 2)
      evalConcrete env store expr `shouldBe` Success (int 10 10)
    it "8 / 0" $ do
      let ?bound = I.Interval (-100) 100
      let expr = BinopExpr (IntConstant 8) Div (IntConstant 0)
      evalConcrete env store expr `shouldBe` Fail (DynamicException (RefVal 0))
    it "3 < 4" $ do
      let ?bound = I.Interval (-100) 100
      let expr = BinopExpr (IntConstant 3) Cmplt (IntConstant 4)
      evalConcrete env store expr `shouldBe` Success (BoolVal true)
    it "3 != 'three'" $ do
      let ?bound = I.Interval (-100) 100
      let expr = BinopExpr (IntConstant 3) Cmpne (StringConstant "three")
      evalConcrete env store expr `shouldBe` Success (BoolVal true)
    it "3.0 % 2.5" $ do
      let ?bound = I.Interval (-100) 100
      let expr = BinopExpr (FloatConstant 3.0) Mod (FloatConstant 2.5)
      evalConcrete env store expr `shouldBe` Success (FloatVal 0.5)
    it "new boolean" $ do
      let ?bound = I.Interval (-100) 100
      let expr = NewExpr BooleanType
      evalConcrete env store expr `shouldBe` Success (BoolVal false)
    it "[1, 2, 3][2]" $ do
      let ?bound = I.Interval (-100) 100
      let expr = ArrayRef "xs" (IntConstant 2)
      let nv = [("xs", 1)]
      let st = [(1, RefVal 2),
                (2, ArrayVal (int 1 3) (int 3 3))]
      evalConcrete nv st expr `shouldBe` Success (int 1 3)
    it "newmultiarray (float) [3][2]" $ do
      let ?bound = I.Interval (-100) 100
      let expr = NewMultiArrayExpr FloatType [IntConstant 3, IntConstant 2]
      evalConcrete env store expr `shouldBe` Success (ArrayVal (ArrayVal (FloatVal 0.0) (int 2 2)) (int 3 3))

  describe "Simple Statements" $ do
    it "i0 = 2 + 3; return i0;" $ do
      let ?bound = I.Interval (-100) 100
      let nv = [("i0", 1)]
      let st = [(1, int 0 0)]
      let stmts = [Assign (LocalVar "i0") (BinopExpr (IntConstant 2) Plus (IntConstant 3)),
                   Return (Just (Local "i0"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (int 5 5))
    it "assign non-declared variable" $ do
      let ?bound = I.Interval (-100) 100
      let stmts = [Assign (LocalVar "s") (IntConstant 2)]
      runStatementsConcrete env store stmts `shouldBe` staticException "Variable \"s\" not bounded"
    it "s = 2; xs = newarray (int)[s]; y = lengthof xs; return xs;" $ do
      let ?bound = I.Interval (-100) 100
      let nv = [("s", 0),
                ("xs", 1),
                ("y", 2)]
      let st = [(0, int 0 0),
                (1, NullVal),
                (2, int 0 0)]
      let stmts = [Assign (LocalVar "s") (IntConstant 2),
                   Assign (LocalVar "xs") (NewArrayExpr IntType (Local "s")),
                   Assign (LocalVar "y") (UnopExpr Lengthof (Local "xs")),
                   Return (Just (Local "y"))]
      runStatementsConcrete nv st stmts `shouldBe` Success (Just (int 2 2))
    it "if 2 <= 3 goto l2; l1: return 1; l2: return 0;" $ do
      let ?bound = I.Interval (-100) 100
      let stmts = [If (BinopExpr (IntConstant 2) Cmple (IntConstant 3)) "l2",
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 0))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (int 0 0))
    it "lookupswitch(4) { case 0: goto l1; case 4: goto l2; default: goto l3;}; l1: return 1; l2: return 2; l3: return 3;" $ do
      let ?bound = I.Interval (-100) 100
      let stmts = [Lookupswitch (IntConstant 4) [(ConstantCase 0, "l1"),
                                                 (ConstantCase 4, "l2"),
                                                 (DefaultCase, "l3")],
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 2)),
                   Label "l3",
                   Return (Just (IntConstant 3))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (int 2 2))
    it "lookupswitch(2) { case 0: goto l1; case 4: goto l2; default: goto l3;}; l1: return 1; l2: return 2; l3: return 3;" $ do
      let ?bound = I.Interval (-100) 100
      let stmts = [Lookupswitch (IntConstant 2) [(ConstantCase 0, "l1"),
                                          (ConstantCase 4, "l2"),
                                          (DefaultCase, "l3")],
                   Label "l1",
                   Return (Just (IntConstant 1)),
                   Label "l2",
                   Return (Just (IntConstant 2)),
                   Label "l3",
                   Return (Just (IntConstant 3))]
      runStatementsConcrete env store stmts `shouldBe` Success (Just (int 3 3))
    it "f0 := @parameter0: float; f1 = f0 * 2.5; return f1; (@parameter0 = 2.0)" $ do
      let ?bound = I.Interval (-100) 100
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
      let ?bound = I.Interval (-100) 4000000
      let files = baseCompilationUnits ++ [factorialExampleFile]
      runProgramConcrete files factorialExampleFile [IntConstant 10] `shouldBe` Success (Just (int 3628800 3628800))
    it "s = new SingleMethodExample; s.x = 2; return s.x" $ do
      let ?bound = I.Interval (-100) 100
      let files = baseCompilationUnits ++ [singleMethodExampleFile]
      runProgramConcrete files singleMethodExampleFile [] `shouldBe` Success (Just (int 2 2))
    it "(-10)! throws IllegalArgumentException" $ do
      let ?bound = I.Interval (-100) 100
      let files = baseCompilationUnits ++ [factorialExampleFile]
      runProgramConcrete files factorialExampleFile [IntConstant (-10)] `shouldBe` dynamicException "java.lang.IllegalArgumentException" "Negative value for argument n"
    it "5 -> [5,10,5,10]" $ do
      let ?bound = I.Interval (-100) 100
      let files = baseCompilationUnits ++ [arrayFieldExampleFile]
      runProgramConcrete files arrayFieldExampleFile [IntConstant 5] `shouldBe` Success (Just (ArrayVal (int 0 10) (int 4 4)))
    it "(new Person(10)).yearsToLive() = 90" $ do
      let ?bound = I.Interval (-100) 100
      let files = baseCompilationUnits ++ [personExampleFile]
      runProgramConcrete files personExampleFile [] `shouldBe` Success (Just (int 90 90))
    it "try { throw e } catch (e) { throw e' }" $ do
      let ?bound = I.Interval (-100) 100
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

    int i j = IntVal $ bounded $ I.Interval i j

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

    unboxMaybe = proc val -> case val of
      Just x -> unbox >>^ (\x -> Just x) -< x
      Nothing -> returnA -< Nothing

    evalConcrete env' store' = runInterp (eval >>> unbox) (testCompilationUnits []) env' store' (testMethod [])

    runStatementsConcrete env' store' stmts =
      runInterp (runStatements >>> unboxMaybe) (testCompilationUnits stmts) env' store' (testMethod stmts) stmts

    runProgramConcrete compilationUnits mainUnit args =
      runInterp (runProgram >>> unboxMaybe) compilationUnits [] [] (mainMethod mainUnit) args

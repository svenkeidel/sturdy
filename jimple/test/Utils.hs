module Utils where

import Data.List
import Data.Maybe

import Syntax

import Classes.Object
import Classes.Throwable
import Classes.IllegalArgumentException
import Classes.ArrayIndexOutOfBoundsException
import Classes.ArithmeticException

baseCompilationUnits :: [CompilationUnit]
baseCompilationUnits = [
  objectFile,
  throwableFile,
  illegalArgumentExceptionFile,
  arrayIndexOutOfBoundsExceptionFile,
  arithmeticExceptionFile]

testMethodBody :: [Statement] -> MethodBody
testMethodBody stmts = FullBody {
  declarations = [],
  statements = stmts,
  catchClauses = []
}

testMethod :: [Statement] -> Method
testMethod stmts = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "test",
  parameters = [],
  throws = [],
  methodBody = testMethodBody stmts
}

testCompilationUnits :: [Statement] -> [CompilationUnit]
testCompilationUnits stmts = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "Test",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [MethodMember (testMethod stmts)]
} : baseCompilationUnits

mainMethod :: CompilationUnit -> Method
mainMethod unit = fromMaybe (error "No entry method found")
  (find (\ m -> methodName m == "main")
    [m | MethodMember m <- fileBody unit])

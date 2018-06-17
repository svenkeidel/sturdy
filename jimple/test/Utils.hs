module Utils where

import Data.List (find)
import Data.Maybe (fromMaybe)

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

mainMethod :: CompilationUnit -> Method
mainMethod unit = fromMaybe (error "No entry method found")
  (find (\ m -> methodName m == "main")
    [m | MethodMember m <- fileBody unit])

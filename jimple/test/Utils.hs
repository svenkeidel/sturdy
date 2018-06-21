module Utils where

import Data.List (find)
import Data.Maybe (fromMaybe)

import Syntax

import Java.Lang.ArithmeticException
import Java.Lang.ArrayIndexOutOfBoundsException
import Java.Lang.ClassCastException
import Java.Lang.IllegalArgumentException
import Java.Lang.NullPointerException
import Java.Lang.Object
import Java.Lang.Throwable

baseCompilationUnits :: [CompilationUnit]
baseCompilationUnits = [
  arithmeticExceptionFile,
  arrayIndexOutOfBoundsExceptionFile,
  classCastExceptionFile,
  illegalArgumentExceptionFile,
  nullPointerExceptionFile,
  objectFile,
  throwableFile]

mainMethod :: CompilationUnit -> Method
mainMethod unit = fromMaybe (error "No entry method found")
  (find (\ m -> methodName m == "main")
    [m | MethodMember m <- fileBody unit])

module LispTypes where

import Data.Ratio
import Data.Complex
import Data.Array
import Data.IORef

import Numeric
import Text.ParserCombinators.Parsec.Error
import Control.Monad.Except
import System.IO

-- Env is a stateful variable (IORef) that holds a map of strings -> IORef LispVal
type Env = IORef[(String, IORef LispVal)]

-- |Lisp Atom data type
data LispVal = Atom String -- Simple Types
    | Number Integer
    | Float Double
    | String String
    | Character Char
    | Bool Bool
    | Ratio Rational -- Composite types
    | Complex (Complex Double)
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Int LispVal)
    -- Stores a primitive function
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    -- Generalized function type
    | Func {
        params :: [String], -- Parameters name
        vararg :: Maybe String, -- name of a variable-length argument list
        body :: [LispVal], -- list of expressions
        closure :: Env -- the environment where the function was created
        }
    | IOFunc ([LispVal] -> IOThrowsError LispVal) -- A dirty function that performs IO
    | Port Handle -- Represents input and output devices

-- Defining Equality methods for LispVal
-- instance Eq LispVal where
--     (==) (Number x)         (Number y)  = x == y
--     (==) (Float x)          (Float y)   = x == y
--     (==) (String x)         (String y)  = x == y
--     (==) (Char x)           (Char y)    = x == y
--     (==) (Bool x)           (Bool y)    = x == y
--     (==) (Ratio x)          (Ratio y)   = x == y
--     (==) (Complex x)        (Complex y) = x == y
--     (==) (DottedList xs x)  (DottedList ys y) =
--         (List $ xs ++ [x]) == (List $ ys ++ [y])
--     (==) (List x) (List y) =
--         (length x == length y) && all (uncurry (==)) (zip x y)

-- instance Ord LispVal where
--     compare (Number x)      (Number y)  = compare x y
--     compare (Float x)       (Float y)   = compare x y
--     compare (String x)      (String y)  = compare x y
--     compare (Bool x)        (Bool y)    = compare x y
--     compare (Ratio x)       (Ratio y)   = compare x y
--     -- There is no compare instance for complex numbers since it violates trichotomy
--     -- https://math.stackexchange.com/questions/257184/defining-the-complex-numbers/257208#257208
--     compare (Complex _)     (Complex _) = error "Complex numbers do not satisfy the axiom of trichotomy"

    -- |Helper function to show a Double in full precision
showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

-- |Print the content of a LispVal
-- Pattern matching is used to destructure
-- The algebraic data type selecting a clause
-- Based on constructors
showVal :: LispVal -> String
-- Simple Types
showVal (Atom name) = "Atom(" ++ name ++ ")"
showVal (String s) = "\"" ++ s ++ "\""
showVal (Number n) = "Number(" ++ show n ++ ")"
showVal (Float f) = "Float(" ++ showFullPrecision f ++ ")"
showVal (Character '\n') = ""
showVal (Character c) = "#\\" ++ [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

-- Composite types
showVal (Ratio r) =
    (show . numerator) r ++ "/" ++ (show . denominator) r
showVal (Complex c) = (showFullPrecision. realPart) c
    ++ (if imagPart c >= 0 then "+" else "")
    ++ (showFullPrecision . imagPart) c ++ "i"
showVal (List l) = "List [" ++ unwordsList l ++ "]"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal v@(Vector _) = "#(" ++ unwordsVector v ++ ")"


-- Show for functions and ports
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func { params = args, vararg = varargs, body = _, closure = _} =
    "(lambda (" ++ unwords args ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
-- |Helper function mapping showVal over a Lisp List
-- |Basically unwords for LispVal

{-
 unwordsList is defined in point-free style
 no argument is specified and it is written only in
 terms of function composition and partial application:
 map is partially applied to showVal (creates a fun that
 takes a list of LispVals and returns a list of their
 string representation). Then, it is composed to
 Haskell's unwords which merges strings in a list
 into a single string separated by spaces.

 This is an important example of currying
-}
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsVector :: LispVal -> String
unwordsVector (Vector y) = unwords . map showVal $ elems y
unwordsVector _ = "not a vector..."

-- |Defining the show method for LispVal
-- See https://www.haskell.org/tutorial/classes.html
-- for details on typeclasses
instance Show LispVal where show = showVal


-- ERRORS

{-
    Some interpreted languages like PHP silently assign
    default values like #f or 0 as a result when errors occur
    This approach means that errors pass silently throughout the
    programs until they can become problematic and need long
    debugging sessions to get rid of the bug completely.

    Here, errors are signaled as soon as they happen and
    immediately break out of execution
-}

data LispError = NumArgs Integer [LispVal]
    | DivideByZero
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

-- Make LispError an instance of Show
showError :: LispError -> String
showError (Default msg) = show msg
showError DivideByZero = show "Division by zero!"
showError (TypeMismatch expected found) = "Invalid type: expected "
    ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (NumArgs expected found) = "Expected " ++ show expected
    ++ " args: found values " ++ unwordsList found

instance Show LispError where show = showError

-- |Type to represent functions that may throw a LispError or return a value.

-- Either is only partially applied (curried) to LispError so that ThrowsError
-- can be used with any data type
-- Either is a monad and the "extra information" carried by it is whether or not
-- An error has occured. This is why haskell does not need a separate
-- Control-flow construct to handle exceptions.
-- Either also provides throwError and catchError
type ThrowsError = Either LispError

-- Needs an Error Monad to handle errors like unbound variables
-- ErrorT is a monad transformer that layers error handling on top of IO
-- May contain IO actions and throws a LispError
-- Curried type constructor that still accepts an argument: return type of the function
type IOThrowsError = ExceptT LispError IO

-- |Convert errors to their string representation and return it
-- trapError action
-- The result of calling trapError is an Either action which will
-- always have valid (Right) data.
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

-- |Extract a value from a ThrowsError type returned by trapError
-- extractValue (Right val) = val
-- extractValue is purposedly left undefined for Left values
extractValue :: ThrowsError a -> Either String a
extractValue (Right val) = Right val
extractValue (Left a) = Left $ show a

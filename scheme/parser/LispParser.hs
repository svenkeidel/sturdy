module LispParser where

import LispTypes

import Control.Monad.Except
import Data.Ratio
import Data.Array
import Numeric


import Text.Parsec.Char hiding (spaces)
import Text.ParserCombinators.Parsec hiding (spaces)

-- |Parse an expression
-- Parse and evaluate a LispVal returning a monadic value
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> do
      return val

-- |Parse a single expression
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow mainParser

-- |Parse multiple expressions
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy mainParser whitespace)

-- | Discard leading whitespace
mainParser :: Parser LispVal
mainParser = do
    skipMany whitespace
    skipMany parseComment
    parseExpr

-- |Parser that recognizes one of the symbols allowed in Scheme Ident.
symbol :: Parser Char
symbol = oneOf "#!$%&|*/+-:<=?>@^_~"

whitespace :: Parser ()
whitespace = skipMany1 (space <|> tab)

-- |Parser to ignore whitespace
spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom ( first : rest )

-- |Parse a boolean value
parseBool :: Parser LispVal
parseBool = do
    _ <- try $ char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


-- Exercise 2.2 and 2.3
-- Parse escaped Chars in strings
parseEscapedChars :: Parser Char
parseEscapedChars = do
    _ <- char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> x

-- |Parse a string
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (parseEscapedChars <|> noneOf "\"")
    _ <- char '"'
    return $ String x

-- |Parse a negative sign Char and return an
-- Integer value to be multiplied to a parsed number
parseIntegerSign :: Parser Integer
parseIntegerSign = do
    signChar <- optionMaybe $ oneOf "+-"
    return $ case signChar of
        Just '-' -> -1
        Just '+' -> 1
        _ -> 1

-- |Parse a negative sign Char and return an
-- Integer value to be multiplied to a parsed number
parseDoubleSign :: Parser Double
parseDoubleSign = do
    signChar <- optionMaybe $ oneOf "+-"
    return $ case signChar of
        Just '-' -> -1
        Just '+' -> 1
        _ -> 1

-- |Same as parseDoubleSign but fail if sign is not found
parseDoubleSignStrict :: Parser Double
parseDoubleSignStrict = do
    signChar <- optionMaybe $ oneOf "+-"
    case signChar of
        Just '-' -> return $ -1
        Just '+' -> return 1
        _ -> fail "no sign"


-- |Parse a Number
{-
    Parse many digits (Parser String)
    Then apply a LispVal Atom Number constructor
    Composed with read to the resulting value
    liftM is used to promote the function to a Monad
-}
parseNumber :: Parser LispVal
parseNumber = parseDecimal
    <|> parseDecimalExplBasis
    <|> parseBinary
    <|> parseOctal
    <|> parseHexadecimal

parseDecimal :: Parser LispVal
parseDecimal = do
    sign <- parseIntegerSign
    Number . (*sign) . read <$> many1 digit

parseDecimalExplBasis :: Parser LispVal
parseDecimalExplBasis = do
    _ <- try $ string "#d"
    sign <- parseIntegerSign
    x <- many1 digit
    (return . Number . (*sign) . read) x

bin2dig :: [Char] -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num t => t -> [Char] -> t
bin2dig' dig "" = dig
bin2dig' dig (x:xs) = bin2dig' (2 * dig + (if x == '0'
    then 0 else 1)) xs

parseBinary :: Parser LispVal
parseBinary = do
    _ <- try $ string "#b"
    sign <- parseIntegerSign
    x <- many1 $ oneOf "01"
    (return . Number . (*sign) . bin2dig) x


parseHexadecimal :: Parser LispVal
parseHexadecimal = do
    _ <- try $ string "#x"
    sign <- parseIntegerSign
    x <- many1 hexDigit
    (return . Number . (*sign) . fst . head . readHex) x

-- oct2dig  = ()
parseOctal :: Parser LispVal
parseOctal = do
    _ <- try $ string "#o"
    sign <- parseIntegerSign
    x <- many1 octDigit
    (return . Number . (*sign) . fst . head . readOct) x

parseFloat :: Parser LispVal
parseFloat = do
    sign <- parseDoubleSign
    x <- many1 digit
    _ <- char '.'
    y <- many1 digit
    (return . Float . (*sign) . fst . head . readFloat) $ x++"."++y

parseRatio :: Parser LispVal
parseRatio = do
    sign <- parseIntegerSign
    x <- many1 digit
    _ <- char '/'
    y <- many1 digit
    (return . Ratio) $ ((*sign) . read) x % read y

-- TODO : remove error
-- |Convert a LispVal Float or Integer to Haskell Double
-- toDouble :: LispVal -> Double
-- toDouble (Float f) = realToFrac f
-- toDouble (Number n) = fromIntegral n
-- toDouble _ = error "not a float or integer" --

-- |Parse a Complex Number
-- parseComplex :: Parser LispVal
-- parseComplex = do
--     realSign <- parseDoubleSign
--     realVal <- try parseFloat <|> parseDecimal
--     imagSign <- parseDoubleSignStrict
--     imagVal <- try parseFloat <|> parseDecimal
--     _ <- char 'i'
--     (return . Complex) $ ((*realSign) . toDouble) realVal :+ ((*imagSign) . toDouble) imagVal

parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do
            x <- anyChar
            notFollowedBy alphaNum
            return [x]
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head value

-- |Parse an Expression (Either a String, a number or an Atom)
parseExpr :: Parser LispVal
parseExpr = do
    expr <- try parseRatio
        -- <|> try parseComplex
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseAtom
        <|> parseString
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseQuoted
        <|> try parseQuasiQuoted
        <|> try parseUnQuote
        <|> try parseVector
        <|> try parseParens
    skipMany parseComment
    return expr

-- |Parse a List of Atoms like a b c d
parseList :: Parser LispVal
parseList = List <$> sepEndBy parseExpr spaces

-- |Parse a Dotted list (a b c . d)
parseDottedList :: Parser LispVal
parseDottedList = do
    -- Parse a List of 0 or more expressions
    -- Separated by spaces
    head_ <- endBy parseExpr spaces
    -- Then parse the remaining Expr after the dot
    tail_ <- char '.' >> spaces >> parseExpr
    skipMany spaces
    return $ DottedList head_ tail_

-- |Parse a Quoted Expression 'a
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- |Parse a QuasiQuoted Expression
-- See https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Atom "unquote", x]


-- |Parse a list expression surrounded by parens
parseParens :: Parser LispVal
parseParens = do
    _ <- char '('
    skipMany space
    x <- try parseDottedList <|> try parseList
    _ <- char ')'
    return x

-- |Parse a Vector #(a b c)
-- |See https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6
parseVector :: Parser LispVal
parseVector = do
    _ <- string "#("
    skipMany space
    vals <- sepEndBy parseExpr spaces
    _ <- char ')'
    return $
        Vector $ listArray (0, length vals -1) vals

-- |Parse a comment like
-- ; this is a lisp comment
parseComment :: Parser ()
parseComment = char ';' >> manyTill anyChar newline >> skipMany space >> return ()

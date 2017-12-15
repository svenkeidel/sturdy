module Data.ATerm where

import           Control.Applicative

import           Data.Attoparsec.Text hiding (string,number)

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.List as L

data ATerm
  = ATerm Text [ATerm]
  | List [ATerm]
  | String Text
  | Number Int

instance Show ATerm where
  show t = case t of
    ATerm c ts -> T.unpack c ++ "(" ++ L.intercalate "," (show <$> ts) ++ ")"
    List ts -> show ts
    String s -> show s
    Number i -> show i

parseATerm :: Text -> Either String ATerm
parseATerm t = case parse aterm t of
  Done _ r -> Right r
  Fail _ _ err -> Left err
  Partial {} -> Left "Partial"

aterm :: Parser ATerm
aterm = constructor <|> constant <|> list <|> string <|> number

constructor :: Parser ATerm
constructor = do
  con <- many1 letter
  _ <- char '('
  ts <- aterm `sepBy` char ','  
  _ <- char ')'
  return $ ATerm (T.pack con) ts

constant :: Parser ATerm
constant = do
  con <- many1 letter
  return $ ATerm (T.pack con) []

list :: Parser ATerm
list = do
  _ <- char '['
  ts <- aterm `sepBy` char ','
  _ <- char ']'
  return $ List ts

string :: Parser ATerm
string = String <$> (char '"' *> concatMany (normalString <|> quotedChar) <* char '"')
  where
    quotedChar :: Parser Text
    quotedChar = do
      e <- char '\\'
      c <- anyChar
      return $ T.pack [e,c]
    
    normalString :: Parser Text
    normalString = takeWhile1 (\c -> c /= '"' && c /= '\\')


concatMany :: (Alternative f, Monoid a) => f a -> f a
concatMany p = mconcat <$> many p

number :: Parser ATerm
number = Number <$> decimal

module Chess.XBoard.Parser
  ( Parser (..),
    literal,
    integer,
    spaces,
    skipSpaces,
    word,
  )
where

import Control.Applicative (Alternative (..))
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Read

newtype Parser a = Parser {runParser :: T.Text -> Maybe (a, T.Text)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (a, rest) -> Just (f a, rest)
      Nothing -> Nothing

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  Parser pf <*> Parser pa = Parser $ \input ->
    case pf input of
      Just (f, rest) -> case pa rest of
        Just (a, rest') -> Just (f a, rest')
        Nothing -> Nothing
      Nothing -> Nothing

instance Monad Parser where
  Parser pf >>= f = Parser $ \input ->
    case pf input of
      Just (rf, rest) -> runParser (f rf) rest
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    p1 input <|> p2 input

literal :: T.Text -> Parser T.Text
literal prefix = Parser $ \text ->
  let match = T.stripPrefix prefix text
   in case match of
        Just suffix -> Just (prefix, suffix)
        Nothing -> Nothing

spaces :: Parser T.Text
spaces = Parser $ \text ->
  Just (T.break (not . C.isSpace) text)

skipSpaces :: Parser ()
skipSpaces = (\_ -> ()) <$> spaces

integer :: Parser Integer
integer = Parser $ \text ->
  let (prefix, suffix) = T.break (not . C.isDigit) text
   in (\x -> (x, suffix)) <$> (Text.Read.readMaybe . T.unpack) prefix

word :: Parser T.Text
word = Parser $ \text ->
  let (prefix, suffix) = T.break C.isSpace text
   in if T.null prefix
        then
          Nothing
        else
          Just (prefix, suffix)

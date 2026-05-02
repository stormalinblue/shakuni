module Chess.XBoard.Parser () where

import Control.Applicative (Alternative (..))
import qualified Data.Text as T

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

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    p1 input <|> p2 input

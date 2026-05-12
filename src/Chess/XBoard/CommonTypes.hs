module Chess.XBoard.CommonTypes
  ( XBoardResultType (..),
    XBoardResult (..),
    XBoardMove (..),
  )
where

import Chess.Position (Position)
import qualified Data.Text as T

data XBoardResultType
  = WhiteWins
  | BlackWins
  | Draw
  | Unfinished
  deriving (Show)

data XBoardResult = XBoardResult
  { result :: XBoardResultType,
    comment :: Maybe T.Text
  }
  deriving (Show)

data XBoardMove
  = Algebraic (Position, Position) (Maybe T.Text)

instance Show XBoardMove where
  show (Algebraic (p1, p2) Nothing) = mconcat [show p1, show p2]
  show (Algebraic (p1, p2) (Just promotion)) = mconcat [show p1, show p2, T.unpack promotion]

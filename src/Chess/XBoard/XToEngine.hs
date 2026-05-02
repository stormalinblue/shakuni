module Chess.XBoard.XToEngine (XToEngine (..), XBoardMove (..), XBoardResult (..)) where

import qualified Data.Text as T

data XBoardPosition = XBoardPosition

data XBoardMove = XBoardMove T.Text

data XBoardResultType
  = WhiteWins
  | BlackWins
  | Draw
  | Unfinished

data XBoardResult = XBoardResult
  { result :: XBoardResultType,
    comment :: Maybe T.Text
  }

data XBoardMoveSelect = SelectMove XBoardMove | SelectAll

data XToEngine
  = XBoard
  | Protover {version :: Int}
  | Accepted
  | Rejected
  | New
  | Variant {name :: T.Text}
  | Quit
  | Random
  | Force
  | Go
  | PlayOther
  | White
  | Black
  | Level {mps :: Int, base :: Int, inc :: Int}
  | SetTime {time :: Int}
  | SetDept {depth :: Int}
  | NodePerSec {nodeRate :: Int}
  | Time {centiseconds :: Int}
  | OpponentTime {centiseconds :: Int}
  | Move XBoardMove
  | UserMove XBoardMove
  | MoveNow
  | Ping {n :: Int}
  | DrawOffered
  | Result XBoardResult
  | SetBoard T.Text
  | Edit T.Text
  | Hint
  | Book
  | Undo
  | RetractMove
  | Hard
  | Easy
  | Post
  | NoPost
  | Analyze
  | Name T.Text
  | Rating {engineRating :: Int, opponentRating :: Int}
  | ICS {hostName :: T.Text}
  | Computer
  | Pause
  | Resume
  | Memory {megabytes :: Int}
  | Cores {ncores :: Int}
  | EndGameTablePath {egtPath :: FilePath, egtType :: T.Text}
  | Option {optionName :: T.Text, optionValue :: T.Text}
  | Exclude XBoardMoveSelect
  | Include XBoardMoveSelect
  | SetScore {score :: Int, scoreDepth :: Int}
  | Lift XBoardPosition
  | Put XBoardPosition
  | Hover XBoardPosition
  | Partner {partnerName :: T.Text}
  | NoPartner
  | PTell T.Text
  | Holding T.Text T.Text
  | HoldingAfter T.Text T.Text T.Text

module Chess.XBoard.XToEngine (XToEngine (..), parseXToEngine, literalP, prefixedP) where

import Chess.Position (Position (..))
import qualified Chess.Position as Pos
import Chess.XBoard.CommonTypes (XBoardMove (..), XBoardResult)
import Chess.XBoard.Parser
import Control.Applicative
import Data.Char (ord)
import qualified Data.Text as T

type Centiseconds = Int

type Hostname = T.Text

data EndgameTableSpec = EndgameTableSpec
  { name :: T.Text,
    path :: FilePath
  }
  deriving (Show)

type Rating = Int

data XBoardScoreSetting = XBoardScoreSetting
  { score :: Int,
    scoreDepth :: Int
  }
  deriving (Show)

data XBoardLevelSetting = XBoardLevelSetting
  { movesPerControl :: Int,
    baseTime :: Centiseconds,
    incrementTime :: Centiseconds
  }
  deriving (Show)

type Megabytes = Int

data XBoardMoveSelect
  = SelectMove XBoardMove
  | SelectAll
  deriving (Show)

data XBoardPosition = XBoardPosition deriving (Show)

data XBoardRatingPair = XBoardRatingPair
  { engineRating :: Rating,
    opponentRating :: Rating
  }
  deriving (Show)

data XBoardOptionSetting = XBoardOptionSetting
  { optionName :: T.Text,
    optionValue :: T.Text
  }
  deriving (Show)

type VariantName = T.Text

type Version = Int

data XToEngine
  = XBoard
  | Protover Version
  | Accepted T.Text
  | Rejected T.Text
  | New
  | Variant VariantName
  | Quit
  | Random
  | Force
  | Go
  | PlayOther
  | White
  | Black
  | Level XBoardLevelSetting
  | SetTime Centiseconds
  | SetDepth Int
  | NodePerSec Int
  | Time Centiseconds
  | OpponentTime Centiseconds
  | Move XBoardMove
  | UserMove XBoardMove
  | MoveNow
  | Ping Int
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
  | Rating XBoardRatingPair
  | ICS Hostname
  | Computer
  | Pause
  | Resume
  | Memory Megabytes
  | Cores Int
  | EndGameTablePath EndgameTableSpec
  | Option XBoardOptionSetting
  | Exclude XBoardMoveSelect
  | Include XBoardMoveSelect
  | SetScore XBoardScoreSetting
  | Lift XBoardPosition
  | Put XBoardPosition
  | Hover XBoardPosition
  | Partner T.Text
  | NoPartner
  | PTell T.Text
  | Holding T.Text T.Text
  | HoldingAfter T.Text T.Text T.Text
  deriving (Show)

literalP :: a -> T.Text -> Parser a
literalP t l = pure t <* literal l

prefixedP :: (b -> a) -> T.Text -> Parser b -> Parser a
prefixedP f t p = f <$> ((literal t) *> spaces *> p)

readInt :: Parser Int
readInt = fromIntegral <$> integer

data Clock = Clock {minutes :: Int, seconds :: Int}

parseXBoardLevelSetting :: Parser XBoardLevelSetting
parseXBoardLevelSetting = do
  -- TODO: Handle notation like 20:30+5 for 42+3 for the
  -- base time (see docs)

  mpc <- readInt
  skipSpaces
  bTime <- clockToCenti <$> parseBase
  skipSpaces
  incrTime <- clockToCenti <$> parseIncr
  return $
    ( XBoardLevelSetting
        { movesPerControl = mpc,
          baseTime = bTime,
          incrementTime = incrTime
        }
    )
  where
    parseBase =
      let justMinutes = do
            mins <- readInt
            return $ Clock {minutes = mins, seconds = 0}
       in minutesAndSeconds <|> justMinutes

    parseIncr =
      let justSeconds = do
            secs <- readInt
            return $ Clock {minutes = 0, seconds = secs}
       in minutesAndSeconds <|> justSeconds

    clockToCenti x = (100 * (60 * minutes x + seconds x))

    minutesAndSeconds = do
      mins <- readInt
      _ <- literal ":"
      secs <- readInt
      return $ Clock {minutes = mins, seconds = secs}

move :: Parser XBoardMove
move = do
  let rankNum = (\x -> fromIntegral (1 + ord x - ord '1')) <$> charInRange '1' '8'
      posPair = do
        fl <- charInRange 'a' 'h'
        rk <- rankNum
        case Pos.fromPartsMaybe fl rk of
          Just pos -> return pos
          Nothing -> Parser $ \_ -> Nothing
  pos1 <- posPair
  pos2 <- posPair
  let partialMove = Algebraic (pos1, pos2)
  partialMove
    <$> ( ( do
              promotion <- letters
              return (Just promotion)
          )
            <|> (return Nothing)
        )

parseXToEngine :: Parser XToEngine
parseXToEngine =
  let parseXBoard = literalP XBoard "xboard"
      parseProtover = prefixedP Protover "protover" readInt
      parseAccepted = prefixedP Accepted "accepted" word
      parseRejected = prefixedP Rejected "rejected" word
      parseNew = literalP New "new"
      parseVariant = prefixedP Variant "variant" word
      parseQuit = literalP Quit "quit"
      parseRandom = literalP Random "random"
      parseForce = literalP Force "force"
      parseGo = literalP Go "go"
      parsePlayOther = literalP PlayOther "playother"
      parseWhite = literalP White "white"
      parseBlack = literalP Black "black"
      parseLevel = prefixedP Level "level" parseXBoardLevelSetting
      parseSetTime = prefixedP SetTime "st" ((100 *) <$> readInt)
      parseSetDepth = prefixedP SetDepth "sd" readInt
      parseNodePerSec = prefixedP NodePerSec "nps" readInt
      parseTime = prefixedP Time "time" ((100 *) <$> readInt)
      parseOpponentTime = prefixedP OpponentTime "otim" ((100 *) <$> readInt)
      parsePost = literalP Post "post"
      parseHard = literalP Hard "hard"
      parseUserMove = prefixedP UserMove "usermove" move
      parsePing = prefixedP Ping "ping" readInt
   in parseXBoard
        <|> parseProtover
        <|> parseAccepted
        <|> parseRejected
        <|> parseNew
        <|> parseVariant
        <|> parseQuit
        <|> parseRandom
        <|> parseForce
        <|> parseGo
        <|> parsePlayOther
        <|> parseWhite
        <|> parseBlack
        <|> parseLevel
        <|> parseSetTime
        <|> parseSetDepth
        <|> parseNodePerSec
        <|> parseTime
        <|> parseOpponentTime
        <|> parsePost
        <|> parseUserMove
        <|> parseHard
        <|> parsePing

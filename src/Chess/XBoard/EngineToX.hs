module Chess.XBoard.EngineToX
  ( EngineToX (..),
    XBoardControl (..),
    XBoardFeature (..),
    XBoardOptionDefn (..),
    ControlRange (..),
    commandText,
  )
where

import Chess.XBoard.CommonTypes (XBoardMove (..), XBoardResult)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data ControlRange = ControlRange
  { value :: Int,
    minValue :: Int,
    maxValue :: Int
  }

data XBoardControl
  = Button
  | Save
  | Reset
  | Check T.Text
  | String T.Text
  | Spin ControlRange
  | Combo (NE.NonEmpty T.Text)
  | Slider ControlRange
  | File T.Text
  | Path T.Text

data XBoardOptionDefn
  = XBoardOption
  { name :: T.Text,
    control :: XBoardControl
  }

data XBoardFeature
  = PingF Bool
  | SetBoard Bool
  | PlayOther Bool
  | SAN Bool
  | UserMove Bool
  | Time Bool
  | Draw Bool
  | SigInt Bool
  | SigTerm Bool
  | ReUse Bool
  | Analyze Bool
  | MyName T.Text
  | Variants [T.Text]
  | Colors Bool
  | ICS Bool
  | Name Bool
  | Pause Bool
  | NodePerSec Bool
  | Debug Bool
  | Memory Bool
  | SMP Bool
  | EndGameTableFormats [T.Text]
  | Option XBoardOptionDefn
  | Exclude Bool
  | SetScore Bool
  | Highlight Bool
  | Done Bool

data EngineToX
  = Feature [XBoardFeature]
  | IllegalMove XBoardMove (Maybe T.Text)
  | ErrorMsg T.Text (Maybe T.Text)
  | Move XBoardMove
  | Result XBoardResult
  | Resign
  | OfferDraw
  | TellOpponent T.Text
  | TellOthers T.Text
  | TellAll T.Text
  | TellUser T.Text
  | TellUserError T.Text
  | AskUser T.Text T.Text
  | TellICS T.Text
  | TellICSNoAlias T.Text
  | Comment T.Text
  | Pong Int

commandText :: EngineToX -> T.Text
commandText (Move (XBoardMove t)) = "move " <> t
commandText (Feature fs) = case fs of
  [] -> ""
  _ ->
    "feature "
      <> ( T.unwords $ do
             f <- fs
             return $ case f of
               SetBoard b -> boolFeat "setboard" b
               Done b -> boolFeat "done" b
               Variants vs -> stringFeat "variants" (T.intercalate "," vs)
               UserMove b -> boolFeat "usermove" b
               ReUse b -> boolFeat "reuse" b
               Colors b -> boolFeat "colors" b
               Pause b -> boolFeat "pause" b
               NodePerSec b -> boolFeat "nps" b
               Debug b -> boolFeat "debug" b
               SigInt b -> boolFeat "sigint" b
               SigTerm b -> boolFeat "sigterm" b
               _ -> "error"
         )
  where
    feat n v = n <> "=" <> v
    boolFeat n v = feat n (if v then "1" else "0")
    stringFeat n v = feat n ("\"" <> v <> "\"")
commandText _ = "error"

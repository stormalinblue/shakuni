module Chess.XBoard.EngineToX () where

import Chess.XBoard.XToEngine (XBoardMove, XBoardResult)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data XBoardFeatureValue = StringFeat T.Text | IntFeat Int | BoolFeat Bool

data ControlRange = ControlRange {value :: Int, minValue :: Int, maxValue :: Int}

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
  = XBoardOption {name :: T.Text, control :: XBoardControl}

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
  | Done Int

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

module Chess.Position (Rank (..), File (..), Position (..), PositionColor (..), posColor) where

import Data.Ix (Ix (..))

newtype Rank = Rank Int deriving (Eq, Ord, Show)

instance Ix Rank where
  range (Rank (rStart), Rank (rEnd)) =
    Rank <$> range (rStart, rEnd)

  index (Rank (rStart), Rank (rEnd)) (Rank (rTest)) =
    index (rStart, rEnd) rTest

  inRange (Rank (rStart), Rank (rEnd)) (Rank (rTest)) =
    inRange (rStart, rEnd) rTest

  rangeSize (Rank (rStart), Rank (rEnd)) =
    rangeSize (rStart, rEnd)

newtype File = File Int deriving (Eq, Ord, Show)

instance Ix File where
  range (File (fStart), File (fEnd)) =
    File <$> range (fStart, fEnd)

  index (File (fStart), File (fEnd)) (File (fTest)) =
    index (fStart, fEnd) fTest

  inRange (File (fStart), File (fEnd)) (File (fTest)) =
    inRange (fStart, fEnd) fTest

  rangeSize (File (fStart), File (fEnd)) =
    rangeSize (fStart, fEnd)

data Position = Position
  { rank :: Rank,
    file :: File
  }
  deriving (Eq, Ord, Show)

posToTuple :: Position -> (File, Rank)
posToTuple pos = (file pos, rank pos)

instance Ix Position where
  range (p1, p2) =
    [ Position {file = f, rank = r} | (f, r) <- (range (posToTuple p1, posToTuple p2))
    ]

  index (p1, p2) p3 =
    index (posToTuple p1, posToTuple p2) (posToTuple p3)

  inRange (p1, p2) p3 =
    inRange (posToTuple p1, posToTuple p2) (posToTuple p3)

  rangeSize (p1, p2) =
    rangeSize (posToTuple p1, posToTuple p2)

data PositionColor = Light | Dark

posColor :: Position -> PositionColor
posColor (Position {rank = Rank (r), file = File (f)})
  | even (r + f) = Dark
  | otherwise = Light

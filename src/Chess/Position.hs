module Chess.Position
  ( Rank,
    rank,
    File,
    file,
    Position (..),
    PositionColor (..),
    posColor,
    upAlongFile,
    downAlongFile,
    diagBranches,
    knightMoves,
    straightBranches,
    starBranches,
    upRight,
    upLeft,
    downRight,
    downLeft,
  )
where

import Data.Char (ord)
import Data.Ix (Ix (..))
import Data.Word (Word8)
import GHC.Char (chr)

newtype Rank = Rank Word8 deriving (Eq, Ord, Show)

rank :: Word8 -> Rank
rank r
  | r < 1 || r > 8 = error "Bad rank"
  | otherwise = Rank (r)

instance Ix Rank where
  range (Rank (rStart), Rank (rEnd)) =
    Rank <$> range (rStart, rEnd)

  index (Rank (rStart), Rank (rEnd)) (Rank (rTest)) =
    index (rStart, rEnd) rTest

  inRange (Rank (rStart), Rank (rEnd)) (Rank (rTest)) =
    inRange (rStart, rEnd) rTest

  rangeSize (Rank (rStart), Rank (rEnd)) =
    rangeSize (rStart, rEnd)

newtype File = File Word8 deriving (Eq, Ord, Show)

file :: Char -> File
file c
  | ord c < ord 'a' || ord c > ord 'h' = error "Bad file"
  | otherwise = File (1 + fromIntegral ((ord c) - (ord 'a')))

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
  { pRank :: Rank,
    pFile :: File
  }
  deriving (Eq, Ord)

instance Show Position where
  show (Position {pFile = File f, pRank = Rank r}) =
    [chr (ord 'a' - 1 + fromIntegral f), chr (ord '0' + fromIntegral r)]

posToTuple :: Position -> (File, Rank)
posToTuple pos = (pFile pos, pRank pos)

tupleToPos :: (Word8, Word8) -> Position
tupleToPos (f, r) = Position {pFile = File f, pRank = Rank r}

instance Ix Position where
  range (p1, p2) =
    [ Position {pFile = f, pRank = r} | (f, r) <- (range (posToTuple p1, posToTuple p2))
    ]

  index (p1, p2) p3 =
    index (posToTuple p1, posToTuple p2) (posToTuple p3)

  inRange (p1, p2) p3 =
    inRange (posToTuple p1, posToTuple p2) (posToTuple p3)

  rangeSize (p1, p2) =
    rangeSize (posToTuple p1, posToTuple p2)

boardBounds :: (Position, Position)
boardBounds = (tupleToPos (1, 1), tupleToPos (8, 8))

inBoard :: Position -> Bool
inBoard = inRange boardBounds

data PositionColor = Light | Dark

posColor :: Position -> PositionColor
posColor (Position {pRank = Rank (r), pFile = File (f)})
  | even (r + f) = Dark
  | otherwise = Light

upAlongFile :: Position -> [Position]
upAlongFile start@(Position {pRank = Rank rStart}) =
  [ start {pRank = (Rank r)} | r <- [rStart + 1 .. 8]
  ]

downAlongFile :: Position -> [Position]
downAlongFile start@(Position {pRank = Rank rStart})
  | rStart <= 1 = []
  | otherwise =
      [ start {pRank = (Rank r)}
      | r <- [rStart - 1, rStart - 2 .. 1]
      ]

rightAlongRank :: Position -> [Position]
rightAlongRank start@(Position {pFile = File fStart}) =
  [ start {pFile = (File f)} | f <- [fStart + 1 .. 8]
  ]

leftAlongRank :: Position -> [Position]
leftAlongRank start@(Position {pFile = File fStart})
  | fStart <= 1 = []
  | otherwise =
      [ start {pFile = (File f)}
      | f <- [fStart - 1, fStart - 2 .. 1]
      ]

upRight :: Position -> [Position]
upRight (Position {pFile = File f, pRank = Rank r}) =
  takeWhile
    inBoard
    $ [tupleToPos (f + n, r + n) | n <- [1 ..]]

upLeft :: Position -> [Position]
upLeft (Position {pFile = File f, pRank = Rank r}) =
  takeWhile
    inBoard
    $ [tupleToPos (f - n, r + n) | n <- [1 ..]]

downRight :: Position -> [Position]
downRight (Position {pFile = File f, pRank = Rank r}) =
  takeWhile
    inBoard
    $ [tupleToPos (f + n, r - n) | n <- [1 ..]]

downLeft :: Position -> [Position]
downLeft (Position {pFile = File f, pRank = Rank r}) =
  takeWhile
    inBoard
    $ [tupleToPos (f - n, r - n) | n <- [1 ..]]

knightMoves :: Position -> [Position]
knightMoves (Position {pFile = File f, pRank = Rank r}) =
  filter inBoard $ (un <> sz)
  where
    dtwos n
      | n < 2 = [n + 2]
      | n > 6 = [n - 2]
      | otherwise = [n - 2, n + 2]
    dones n
      | n < 1 = [n + 1]
      | n > 1 = [n - 1]
      | otherwise = [n - 1, n + 1]

    un = do
      rn <- dtwos r
      fn <- dones f
      return $ tupleToPos (fn, rn)

    sz = do
      rn <- dones r
      fn <- dtwos f
      return $ tupleToPos (fn, rn)

diagBranches :: Position -> [[Position]]
diagBranches pos =
  map
    ($ pos)
    [ upRight,
      upLeft,
      downRight,
      downLeft
    ]

straightBranches :: Position -> [[Position]]
straightBranches pos =
  map
    ($ pos)
    [ upAlongFile,
      downAlongFile,
      leftAlongRank,
      rightAlongRank
    ]

starBranches :: Position -> [[Position]]
starBranches pos = straightBranches pos <> diagBranches pos

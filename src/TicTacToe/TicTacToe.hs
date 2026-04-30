module TicTacToe.TicTacToe (
  Player(..),
  CellState(..),
  GameState(..),
  GameResult(..),
  DeterminedResult(..),
  initState,
  makeMove,
  availableMoves) where

import Data.Array.IArray

data Player = PlayerX | PlayerO deriving (Eq, Show)

data CellState = Empty | Marker Player deriving (Eq, Show)

data DeterminedResult = Win Player | Tie deriving (Eq, Show)
data GameResult = Undetermined | Determined DeterminedResult deriving (Eq, Show)

data GameState = GameState
  { board :: Array (Int, Int) CellState,
    turn :: Player,
    result :: GameResult
  }
  deriving (Eq, Show)



boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0, 0), (2, 2))

initState :: GameState
initState =
  GameState
    { board = array boardBounds [(ix, Empty) | ix <- range boardBounds],
      turn = PlayerX,
      result = Undetermined
    }

makeMove :: GameState -> (Int, Int) -> GameState
makeMove gs m =
  GameState
    { board = nextBoard,
      turn = nextPlayer (turn gs),
      result = res
    }
  where
    nextPlayer PlayerX = PlayerO
    nextPlayer PlayerO = PlayerX

    nextBoard = (board gs) // [(m, Marker $ turn gs)]

    orResult [] = Undetermined
    orResult (x:xs) = case x of
      Undetermined -> orResult xs
      _ -> x

    pairOr x y = case x of
      Undetermined -> y
      _ -> x

    allEqual :: (Eq a) => [a] -> Bool
    allEqual [] = True
    allEqual x = and $ zipWith (==) (init x) (drop 1 x)

    (row, col) = m

    relCol = [(r, col) | r <- range (0, 2)]
    relRow = [(row, c) | c <- range (0, 2)]
    mainDiag = [(i, i) | i <- range (0, 2)]
    crossDiag = [(2 - i, i) | i <- range (0, 2)]

    lineResult indices = case nextBoard ! (head indices) of
        Empty -> Undetermined
        Marker p -> case allEqual ((nextBoard !) <$> indices) of
          True -> (Determined . Win) p
          False -> Undetermined

    res = orResult [
      lineResult relCol,
      lineResult relRow,
      if (row + col /= 2) then Undetermined else (lineResult crossDiag),
      if (row /= col) then Undetermined else (lineResult mainDiag),
      if all (/= Empty) (elems nextBoard) then (Determined Tie) else Undetermined]


availableMoves :: GameState -> [(Int, Int)]
availableMoves gs =
  case (result gs) of
    Undetermined -> [ix | (ix, e) <- assocs (board gs), e == Empty]
    Determined _ -> []

module TicTacToe.Main (main) where

import Data.Array.IArray

data Player = PlayerX | PlayerO deriving (Eq, Show)

data CellState = Empty | Marker Player deriving (Eq, Show)

data GameState = GameState
  { board :: Array (Int, Int) CellState,
    turn :: Player
  }
  deriving (Eq, Show)

boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0, 0), (2, 2))

initState :: GameState
initState =
  GameState
    { board = array boardBounds [(ix, Empty) | ix <- range boardBounds],
      turn = PlayerX
    }

makeMove :: GameState -> (Int, Int) -> GameState
makeMove gs m =
  GameState
    { board = (board gs) // [(m, Marker $ turn gs)],
      turn = nextPlayer (turn gs)
    }
  where
    nextPlayer PlayerX = PlayerO
    nextPlayer PlayerO = PlayerX

availableMoves :: GameState -> [(Int, Int)]
availableMoves gs =
  [ix | (ix, e) <- assocs (board gs), e == Empty]

getMakeMove :: GameState -> IO ()
getMakeMove gs = do
  print gs
  let am = availableMoves gs
  case am of
    [] -> do
      putStrLn "Game over"
    (m : _) -> do
      print am
      move <- getLine
      getMakeMove (makeMove gs m)

main :: IO ()
main = do
  getMakeMove initState

module TicTacToe.Main (main) where

import TicTacToe.GridShow (gridShow)
import TicTacToe.TicTacToe
import Text.Read (readMaybe)
import Data.List (intercalate)

boardShow :: GameState -> String
boardShow gs = gridShow (cellValue <$> bd)
  where
    bd = board gs

    cellValue (Marker PlayerX) = "X"
    cellValue (Marker PlayerO) = "O"
    cellValue (Empty) = " "

parseMove :: String -> Maybe (Int, Int)
parseMove s = case (words s) of
  [x1, y1] -> do
    x <- (readMaybe x1) :: Maybe Int
    y <- (readMaybe y1) :: Maybe Int
    return (x, y)
  _ -> Nothing

getMove :: [(Int, Int)] -> IO (Int, Int)
getMove am = do
  line <- getLine
  case (parseMove line) of
    Nothing -> do
      putStrLn "Invalid move"
      getMove am
    Just m -> case (elem m am) of
      True -> return m
      False -> do
        putStrLn "Unavailable move"
        getMove am

getMakeMove :: GameState -> IO ()
getMakeMove gs = do
  putStrLn (boardShow gs)
  case (result gs) of
    Undetermined -> do
      putStrLn $ concat ["Player ", if turn gs == PlayerO then "O" else "X", "'s turn."]
      let am = availableMoves gs
      case am of
        [] -> do
          putStrLn "Game over"
        _ -> do
          putStrLn $ intercalate ", " [concat [show x, " ", show y] | (x, y) <- am]
          move <- getMove am
          getMakeMove (makeMove gs move)
    Determined res -> case res of
      Tie -> putStrLn "Tie!"
      Win (p) -> case p of
        PlayerX -> putStrLn "X Wins!"
        PlayerO -> putStrLn "O Wins!"

main :: IO ()
main = do
  getMakeMove initState

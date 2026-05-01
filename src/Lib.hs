module Lib
  ( libMain,
  )
where

import qualified Chess.Main
import qualified Chess.XBoard.Main
import qualified TicTacToe.Main

libMain :: IO ()
libMain = Chess.XBoard.Main.main

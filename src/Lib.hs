module Lib
  ( libMain,
  )
where

import Chess.Chess (chessMain)
import qualified TicTacToe.Main

libMain :: IO ()
libMain = TicTacToe.Main.main

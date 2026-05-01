module Lib
  ( libMain,
  )
where

import qualified Chess.Main
import qualified TicTacToe.Main

libMain :: IO ()
libMain = Chess.Main.main

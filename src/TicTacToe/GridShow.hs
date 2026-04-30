module TicTacToe.GridShow (gridShow) where

import Data.Array.IArray
import Data.List (intersperse)

strjoin :: String -> [String] -> String
strjoin s [] = []
strjoin s [x] = x
strjoin s (x:y:xs) = concat [x, s, strjoin s (y:xs)]


gridShow :: Array (Int, Int) String -> String
gridShow arr =
  let topLeft = '┌'
      topCenter = '┬'
      topRight = '┐'
      botLeft = '└'
      botCenter = '┴'
      botRight = '┘'
      midLeft = '├'
      midCenter = '┼'
      midRight = '┤'
      vert = '│'
      horz = '─'

      decoratorLine l c r widths =
         [l] ++ (strjoin [c] [replicate w horz | w <- widths]) ++ [r]

      topLine = decoratorLine topLeft topCenter topRight
      botLine = decoratorLine botLeft botCenter botRight
      midLine = decoratorLine midLeft midCenter midRight

      ((r0, c0), (r1, c1)) = bounds arr

      pad = 1
      horzSpacing = replicate pad ' '
      horzBetween = horzSpacing ++ [vert] ++ horzSpacing

      colWidths = [2 * pad + maximum [length (arr ! (r, c)) | r <- range (r0, r1)] | c <- range (c0, c1)]
      contentRow rowNum = concat [
        [vert],
        horzSpacing,
        strjoin horzBetween [arr ! (rowNum, c) | c <- range (c0, c1)],
        horzSpacing,
        [vert]]

      result = (unlines . concat) [
        [topLine colWidths],
        intersperse (midLine colWidths) [contentRow r | r <- range (r0, r1)],
        [botLine colWidths ]]

   in result

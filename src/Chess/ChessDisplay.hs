-- Written by Microsoft Copilot
{-# LANGUAGE OverloadedStrings #-}

module Chess.ChessDisplay (printBoard) where

import qualified Chess.GameState.Display as GSD
import Chess.Piece (Color (..), PieceType (..))
import Chess.Position (Position (..), PositionColor (..), posColor)
import Data.Array.IArray

-- Unicode symbols for pieces
unicodePiece :: GSD.DisplayPiece -> String
unicodePiece (GSD.DisplayPiece {GSD.color = color, GSD.pieceType = pieceType}) =
  up (color, pieceType)
  where
    up (White, King) = "♔"
    up (White, Queen) = "♕"
    up (White, Rook) = "♖"
    up (White, Bishop) = "♗"
    up (White, Knight) = "♘"
    up (White, Pawn) = "♙"
    up (Black, King) = "♚"
    up (Black, Queen) = "♛"
    up (Black, Rook) = "♜"
    up (Black, Bishop) = "♝"
    up (Black, Knight) = "♞"
    up (Black, Pawn) = "\x265F\xFE0E"

-- ASCII fallback (optional)
asciiPiece :: GSD.DisplayPiece -> String
asciiPiece dp = case (GSD.pieceType dp) of
  King -> pick 'K'
  Queen -> pick 'Q'
  Rook -> pick 'R'
  Bishop -> pick 'B'
  Knight -> pick 'N'
  Pawn -> pick 'P'
  where
    pick ch = if (GSD.color dp) == White then [ch] else [toEnum (fromEnum ch + 32)] -- lowercase for black

-- Choose unicode by default
renderPiece :: Maybe GSD.DisplayPiece -> String
renderPiece = maybe " " unicodePiece

-- ANSI background colors for squares
lightSquare, darkSquare, reset :: String
lightSquare = "\ESC[48;5;223m" -- light beige
darkSquare = "\ESC[48;5;94m" -- brown
reset = "\ESC[0m"

squareColor :: Position -> String
squareColor pos = case (posColor pos) of
  Light -> lightSquare
  _ -> darkSquare

-- Render a single square
renderSquare :: Position -> Maybe GSD.DisplayPiece -> String
renderSquare pos piece =
  squareColor pos ++ renderPiece piece ++ " " ++ reset

-- Render entire board (8×8 assumed)
printBoard :: GSD.DisplayBoard -> IO ()
printBoard (GSD.DisplayBoard (board)) = do
  let ((f0, r0), (f1, r1)) = bounds board
  mapM_
    ( \r -> do
        mapM_ (\f -> putStr (renderSquare (Position {rank = r, file = f}) (board ! (f, r)))) (range (f0, f1))
        putStrLn ""
    )
    ((reverse . range) (r0, r1))

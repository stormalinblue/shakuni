module Chess.Main (main) where

import Chess.ChessDisplay
import Chess.GameState.Display as GSD
import Chess.Piece
import Chess.Position
import Data.Array.IArray

exampleBoard :: GSD.DisplayBoard
exampleBoard =
  GSD.DisplayBoard $
    array
      bnds
      [((f, r), initialPiece f r) | (f, r) <- range bnds]
  where
    bnds = ((File 1, Rank 1), (File 8, Rank 8))

jdp :: (Color, PieceType) -> Maybe DisplayPiece
jdp (c, pt) = Just (GSD.DisplayPiece {color = c, pieceType = pt})

initialPiece :: File -> Rank -> Maybe GSD.DisplayPiece
initialPiece _ (Rank 2) = jdp (White, Pawn)
initialPiece _ (Rank 7) = jdp (Black, Pawn)
initialPiece (File f) (Rank 1) = case f of
  1 -> jdp (White, Rook)
  2 -> jdp (White, Knight)
  3 -> jdp (White, Bishop)
  4 -> jdp (White, Queen)
  5 -> jdp (White, King)
  6 -> jdp (White, Bishop)
  7 -> jdp (White, Knight)
  8 -> jdp (White, Rook)
  _ -> Nothing
initialPiece (File f) (Rank 8) = case f of
  1 -> jdp (Black, Rook)
  2 -> jdp (Black, Knight)
  3 -> jdp (Black, Bishop)
  4 -> jdp (Black, Queen)
  5 -> jdp (Black, King)
  6 -> jdp (Black, Bishop)
  7 -> jdp (Black, Knight)
  8 -> jdp (Black, Rook)
  _ -> Nothing
initialPiece _ _ = Nothing

main :: IO ()
main = printBoard exampleBoard

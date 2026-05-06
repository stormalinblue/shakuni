module Chess.Main (main) where

import Chess.ChessDisplay
import Chess.GameState.Display as GSD
import Chess.Piece hiding (pieceType)
import Chess.Position
import Data.Array.IArray
import Data.Word (Word8)

exampleBoard :: GSD.DisplayBoard
exampleBoard =
  GSD.DisplayBoard $
    array
      ((file 'a', rank 1), (file 'h', rank 8))
      [((file f, rank r), initialPiece f r) | (f, r) <- range bnds]
  where
    bnds = (('a', 1), ('h', 8))

jdp :: (Color, PieceType) -> Maybe DisplayPiece
jdp (c, pt) = Just (GSD.DisplayPiece {color = c, pieceType = pt})

initialPiece :: Char -> Word8 -> Maybe GSD.DisplayPiece
initialPiece _ 2 = jdp (White, Pawn)
initialPiece _ 7 = jdp (Black, Pawn)
initialPiece f 1 = case f of
  'a' -> jdp (White, Rook)
  'b' -> jdp (White, Knight)
  'c' -> jdp (White, Bishop)
  'd' -> jdp (White, Queen)
  'e' -> jdp (White, King)
  'f' -> jdp (White, Bishop)
  'g' -> jdp (White, Knight)
  'h' -> jdp (White, Rook)
  _ -> Nothing
initialPiece f 8 = case f of
  'a' -> jdp (Black, Rook)
  'b' -> jdp (Black, Knight)
  'c' -> jdp (Black, Bishop)
  'd' -> jdp (Black, Queen)
  'e' -> jdp (Black, King)
  'f' -> jdp (Black, Bishop)
  'g' -> jdp (Black, Knight)
  'h' -> jdp (Black, Rook)
  _ -> Nothing
initialPiece _ _ = Nothing

main :: IO ()
main = printBoard exampleBoard

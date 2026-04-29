module Chess.GameState.Display (DisplayPiece(..), DisplayBoard(..)) where

import Chess.Position
import Chess.Piece
import Data.Array.IArray

data DisplayPiece = DisplayPiece { color :: Color, pieceType :: PieceType }
newtype DisplayBoard = DisplayBoard (Array (File, Rank) (Maybe DisplayPiece))

module Chess.Piece (PieceType (..), Color (..), PieceState) where

import Chess.Position

data PieceType = King | Queen | Knight | Rook | Bishop | Pawn deriving (Eq, Ord, Show)

data PieceState
  = NonCastlingState {pieceType :: PieceType, position :: Position}
  | CastlingPieceState {canCastle :: Bool, pieceType :: PieceType, position :: Position}
  | EnPassantPieceState {everMoved :: Bool, justMoved2 :: Bool, pieceType :: PieceType, position :: Position}

data Color = White | Black deriving (Eq, Ord, Show)

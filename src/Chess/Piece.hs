module Chess.Piece
  ( PieceType (..),
    piecePos,
    withPos,
    BasicPieceState (..),
    Color (..),
    normalInit,
    PieceState (..),
  )
where

import Chess.Position

data PieceType = King | Queen | Knight | Rook | Bishop | Pawn deriving (Eq, Ord, Show)

data BasicPieceState = BasicPieceState {pieceType :: PieceType, position :: Position}

instance Show BasicPieceState where
  show (BasicPieceState {pieceType = pt, position = pos}) =
    show pt <> " @ " <> show pos

data CastlingPieceState = CastlingPieceState {canCastle :: Bool} deriving (Show)

data EnPassantPieceState = EnPassantPieceState
  { justMoved2 :: Bool,
    everMoved :: Bool
  }
  deriving (Show)

data PieceState
  = NonCastlingPS BasicPieceState
  | CastlingPS BasicPieceState CastlingPieceState
  | EnPassantPS BasicPieceState EnPassantPieceState

instance Show PieceState where
  show (NonCastlingPS bps) = show bps
  show (CastlingPS bps cps) = show bps <> (if canCastle cps then " castles" else "")
  show (EnPassantPS bps eps) = show bps <> suffix
    where
      suffix
        | (not . everMoved) eps = " untouched"
        | justMoved2 eps = " en passants"
        | otherwise = ""

piecePos :: PieceState -> Position
piecePos (NonCastlingPS bps) = position bps
piecePos (CastlingPS bps _) = position bps
piecePos (EnPassantPS bps _) = position bps

withPos :: PieceState -> Position -> PieceState
withPos (NonCastlingPS bps) pos = NonCastlingPS (bps {position = pos})
withPos (CastlingPS bps x) pos = CastlingPS (bps {position = pos}) x
withPos (EnPassantPS bps x) pos = EnPassantPS (bps {position = pos}) x

normalInit :: PieceType -> Position -> PieceState
normalInit pt pos =
  let nInit King pos2 = castleInit King pos2
      nInit Rook pos2 = castleInit Rook pos2
      nInit Queen pos2 = nCastleInit Queen pos2
      nInit Knight pos2 = nCastleInit Knight pos2
      nInit Bishop pos2 = nCastleInit Bishop pos2
      nInit Pawn pos2 = enPassInit Pawn pos2

      bps pt2 pos2 = BasicPieceState {pieceType = pt2, position = pos2}

      castleInit pt2 pos2 =
        CastlingPS
          (bps pt2 pos2)
          ( CastlingPieceState
              { canCastle = True
              }
          )

      nCastleInit pt2 pos2 =
        NonCastlingPS
          (bps pt2 pos2)

      enPassInit pt2 pos2 =
        EnPassantPS
          (bps pt2 pos2)
          ( EnPassantPieceState
              { everMoved = False,
                justMoved2 = False
              }
          )
   in nInit pt pos

data Color = White | Black deriving (Eq, Ord, Show)

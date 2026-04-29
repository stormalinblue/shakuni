module Chess.GameState (
    BoardData,
    Color,
    PieceType) where

import Chess.Position
import Chess.Piece

import qualified Chess.GameState.Display as GSD


data BaseMove = BaseMove { from :: Position, to :: Position }
newtype Move = Move [BaseMove]

data PlayerState = PlayerState { player :: Color, pieces :: [PieceState] }


data BoardData = BoardData {
    whitePlayer :: PlayerState,
    blackPlayer :: PlayerState,
    turn :: Color
    -- halfMoveClock :: Int,
    -- fullMoveNumber :: Int
}

class Board b where
    isGameOver :: b -> Bool
    -- turn :: b -> Color
    castlingRights :: b -> [PieceState]
    fullMoveNumber :: b -> Int
    halfMoveClock :: b -> Int
    legalMoves :: b -> [Move]
    isCheck :: b -> Bool
    givesCheck :: b -> Move -> Bool
    givesCheckmate :: b -> Move -> Bool
    isLegal :: b -> Move -> Bool
    isCheckmate :: b -> Bool
    canClaimDraw :: b -> Bool

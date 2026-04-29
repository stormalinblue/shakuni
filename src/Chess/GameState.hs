module Chess.GameState
  ( GameState,
    Color,
    PieceType,
  )
where

import qualified Chess.GameState.Display as GSD
import Chess.Piece
import Chess.Position

data BaseMove = BaseMove {from :: Position, to :: Position}

newtype Move = Move [BaseMove]

data PlayerState = PlayerState {player :: Color, pieces :: [PieceState]}

data GameState = GameState
  { whitePlayer :: PlayerState,
    blackPlayer :: PlayerState,
    turn :: Color,
    halfMoveClock :: Int,
    fullMoveNumber :: Int
  }

makeMove :: GameState -> Move -> GameState
makeMove gs m =
  gs
    { turn = nextPlayer (turn gs),
      fullMoveNumber = nextFMN (turn gs) (fullMoveNumber gs)
    }
  where
    nextPlayer White = Black
    nextPlayer Black = White

    nextFMN :: Color -> Int -> Int
    nextFMN White x = x
    nextFMN Black x = 1 + x

-- class Board b where
--     isGameOver :: b -> Bool
--     -- turn :: b -> Color
--     castlingRights :: b -> [PieceState]
--     fullMoveNumber :: b -> Int
--     halfMoveClock :: b -> Int
--     legalMoves :: b -> [Move]
--     isCheck :: b -> Bool
--     givesCheck :: b -> Move -> Bool
--     givesCheckmate :: b -> Move -> Bool
--     isLegal :: b -> Move -> Bool
--     isCheckmate :: b -> Bool
--     canClaimDraw :: b -> Bool

module Chess.GameState
  ( GameState (..),
    Move (..),
    initState,
    makeMove,
    makeNullMove,
    availableMoves,
  )
where

import Chess.Piece
import Chess.Position
import Data.Array.IArray (range)

data Move = Move {pickUp :: [Position], putDown :: [(Color, PieceState)]} deriving (Show)

data PlayerState = PlayerState {player :: Color, pieces :: [PieceState]} deriving (Show)

data GameState = GameState
  { whitePlayer :: PlayerState,
    blackPlayer :: PlayerState,
    turn :: Color
  }
  deriving (Show)

initState :: GameState
initState =
  GameState
    { whitePlayer =
        PlayerState
          { player = White,
            pieces = (notPawns 1) <> (pawns 2)
          },
      blackPlayer =
        PlayerState
          { player = Black,
            pieces = (notPawns 8) <> (pawns 7)
          },
      turn = White
    }
  where
    pawns r = [normalInit Pawn (pos f r) | f <- range ('a', 'h')]
    notPawns r =
      (\(pt, p) -> normalInit pt (p r))
        <$> [ (Rook, pos 'a'),
              (Knight, pos 'b'),
              (Bishop, pos 'c'),
              (Queen, pos 'd'),
              (King, pos 'e'),
              (Bishop, pos 'f'),
              (Knight, pos 'g'),
              (Rook, pos 'h')
            ]

    pos f r =
      Position {pFile = (file f), pRank = (rank r)}

currentPlayerState :: GameState -> PlayerState
currentPlayerState gs = case turn gs of
  White -> whitePlayer gs
  Black -> blackPlayer gs

opponentState :: GameState -> PlayerState
opponentState gs = case turn gs of
  White -> blackPlayer gs
  Black -> whitePlayer gs

availableMoves :: GameState -> [Move]
availableMoves gs =
  concatMap aMoves ps
  where
    us = currentPlayerState gs
    them = opponentState gs

    ps = pieces us

    ops = pieces them

    occupancy pStates =
      piecePos <$> pStates

    weOccupy pos = pos `elem` occupancy ps
    theyOccupy pos = pos `elem` occupancy ops
    occupied pos = weOccupy pos || theyOccupy pos

    tw2 :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
    tw2 _ _ [] = []
    tw2 beforePred whenPred (x : xs) =
      case (beforePred x, whenPred x) of
        (True, True) -> x : tw2 beforePred whenPred xs
        (True, False) -> [x]
        (False, _) -> []

    targetPos pState = case pState of
      NonCastlingPS bps -> case pieceType bps of
        Knight -> filter (not . weOccupy) (knightMoves pos)
        Rook -> fromBranches (straightBranches pos)
        Bishop -> fromBranches (diagBranches pos)
        Queen -> fromBranches (starBranches pos)
        King -> fromBranches (diagBranches pos)
        _ -> []
        where
          pos = position bps
      EnPassantPS bps _ -> case pieceType bps of
        Pawn ->
          takeWhile (not . occupied) (pawnAhead)
            <> filter (theyOccupy) (pawnDiags)
        _ -> []
        where
          pos = position bps
          pawnAhead = case turn gs of
            White -> take 2 $ upAlongFile pos
            Black -> take 2 $ downAlongFile pos

          pawnDiags = case turn gs of
            White -> concatMap (take 1 . ($ pos)) [upRight, upLeft]
            Black -> concatMap (take 1 . ($ pos)) [downLeft, downRight]
      CastlingPS bps _ -> case pieceType bps of
        Rook ->
          fromBranches (straightBranches pos)
        King ->
          filter (not . weOccupy) (concatMap (take 1) (starBranches pos))
        _ -> []
        where
          pos = position bps
      where
        fromBranches = concatMap trimBranches
        trimBranches = (tw2 (not . weOccupy) (not . theyOccupy))

    aMoves pState =
      [ Move {pickUp = [piecePos pState] <> (if occupied tpos then [tpos] else []), putDown = [(turn gs, pState `withPos` tpos)]}
      | tpos <- targetPos pState
      ]

nextPlayer :: Color -> Color
nextPlayer White = Black
nextPlayer Black = White

makeMove :: GameState -> Move -> GameState
makeMove gs m =
  evolve
  where
    removePickedUp playerS =
      playerS {pieces = filter (\ps -> not $ piecePos ps `elem` (pickUp m)) (pieces playerS)}

    insertPutDown playerS =
      playerS {pieces = (pieces playerS) <> (map snd (putDown m))}

    evolve = case turn gs of
      White ->
        GameState
          { turn = nextPlayer (turn gs),
            whitePlayer = (insertPutDown . removePickedUp) (whitePlayer gs),
            blackPlayer = removePickedUp (blackPlayer gs)
          }
      Black ->
        GameState
          { turn = nextPlayer (turn gs),
            whitePlayer = removePickedUp (whitePlayer gs),
            blackPlayer = (insertPutDown . removePickedUp) (blackPlayer gs)
          }

makeNullMove :: GameState -> GameState
makeNullMove gs = gs {turn = nextPlayer (turn gs)}

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

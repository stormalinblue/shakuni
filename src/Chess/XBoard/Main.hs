module Chess.XBoard.Main (main) where

import Chess.GameState
  ( GameState,
    Move (..),
    availableMoves,
    initState,
    makeMove,
    makeNullMove,
  )
import Chess.Piece (piecePos)
import Chess.XBoard.CommonTypes (XBoardMove (..))
import qualified Chess.XBoard.EngineToX as E2X
import Chess.XBoard.Parser (runParser)
import qualified Chess.XBoard.XToEngine as X2E
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
  )

data EngineState
  = ExpectXBoard
  | ExpectProtoVer
  | DumpFeatures
  | Ready
  | ParseError
  | Quitting
  | InGame GameState
  deriving (Show)

getLogLine :: IO T.Text
getLogLine = do
  line <- TIO.getLine
  TIO.putStrLn $
    T.concat
      ["#", " (input) ", line]
  -- hPutStr stderr line
  return line

type EngineM = StateT EngineState IO

parseLine :: T.Text -> Maybe X2E.XToEngine
parseLine line = fst <$> runParser X2E.parseXToEngine (T.strip line)

debugPrintLn :: T.Text -> EngineM ()
debugPrintLn out = do
  engineState <- get
  case engineState of
    ExpectXBoard -> return ()
    ExpectProtoVer -> return ()
    DumpFeatures -> return ()
    _ -> liftIO $ TIO.putStr (T.unlines (("# " <>) <$> T.lines out))

moveToXBM :: Move -> XBoardMove
moveToXBM m =
  XBoardMove (T.show start <> T.show end)
  where
    start = head (pickUp m)
    end = (piecePos . snd . head) (putDown m)

xBoardLoop :: EngineM (ExitCode)
xBoardLoop =
  let handleCommand subRoutine = do
        rawLine <- liftIO getLogLine
        case parseLine rawLine of
          Just X2E.Quit -> do
            put Quitting
          x -> subRoutine x
        xBoardLoop

      sendCommand cmd =
        do
          liftIO $
            TIO.putStrLn
              (E2X.commandText cmd)
   in do
        status <- get
        case status of
          ExpectXBoard -> do
            handleCommand $ \_ -> do
              put ExpectProtoVer
          ExpectProtoVer -> do
            handleCommand $ \protoVerLine -> do
              case protoVerLine of
                Just (X2E.Protover _) ->
                  put DumpFeatures
                _ -> put ParseError
          DumpFeatures -> do
            sendCommand (E2X.Feature [E2X.Done False])
            sendCommand
              ( E2X.Feature
                  [ E2X.Variants ["normal"],
                    E2X.UserMove True,
                    E2X.ReUse False,
                    E2X.Colors False,
                    E2X.Pause False,
                    E2X.NodePerSec False,
                    E2X.Debug True,
                    E2X.SigInt False,
                    E2X.SigTerm False
                  ]
              )

            liftIO $ TIO.putStrLn "feature myname=\"Shakuni\" memory=0 smp=0 ping=0"
            sendCommand (E2X.Feature [E2X.Done True])
            put Ready
            xBoardLoop
          ParseError -> do
            return $ ExitFailure 1
          Ready -> do
            handleCommand $ \cmd -> do
              debugPrintLn (T.show cmd)
              case cmd of
                Just X2E.New -> do
                  put (InGame initState)
                _ -> return ()
          InGame gs -> do
            handleCommand $ \cmd -> do
              case cmd of
                Just (X2E.UserMove _) -> do
                  let currentState = makeNullMove gs
                  put (InGame $ currentState)
                  debugPrintLn (T.show currentState)
                  let moves = availableMoves currentState
                  debugPrintLn (T.show moves)
                  case moves of
                    [] -> return ()
                    (x : _) -> do
                      put (InGame (makeMove currentState x))
                      debugPrintLn (T.show moves)
                      sendCommand (E2X.Move (moveToXBM x))
                _ -> return ()
          Quitting -> do
            debugPrintLn ("Quitting")
            return ExitSuccess

xBoardMain :: EngineM (ExitCode)
xBoardMain = do
  xBoardLoop

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (exitCode, _) <- runStateT xBoardMain (ExpectXBoard)
  exitWith exitCode

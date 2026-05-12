module Chess.XBoard.Main (main) where

import Chess.GameState
  ( GameState (GameState),
    Move (..),
    availableMoves,
    initState,
    makeMove,
    makeNullMove,
  )
import Chess.Piece (piecePos)
import Chess.Position (fileLetter, rankNumber)
import Chess.XBoard.CommonTypes (XBoardMove (..))
import qualified Chess.XBoard.EngineToX as E2X
import Chess.XBoard.Parser (runParser)
import qualified Chess.XBoard.XToEngine as X2E
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc.IO (threadDelay)
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
  )

data XBoardParseState
  = ExpectXBoard
  | ExpectProtoVer
  | DumpFeatures
  | Ready
  | ParseError
  | Quitting
  | InGame GameState
  deriving (Show)

data EngineState
  = EngineState
  { parseState :: XBoardParseState,
    shouldPonder :: Bool
  }
  deriving (Show)

getLogLine :: IO T.Text
getLogLine = do
  line <- TIO.getLine
  TIO.putStrLn $
    T.unwords
      ["#", "(input)", line]
  -- hPutStr stderr line
  return line

type EngineM = StateT EngineState IO

parseLine :: T.Text -> Maybe X2E.XToEngine
parseLine line = fst <$> runParser X2E.parseXToEngine (T.strip line)

debugPrintLn :: T.Text -> EngineM ()
debugPrintLn out = do
  engineState <- getParseState
  case engineState of
    ExpectXBoard -> return ()
    ExpectProtoVer -> return ()
    DumpFeatures -> return ()
    _ -> liftIO $ TIO.putStr (T.unlines (("# " <>) <$> T.lines out))

moveToXBM :: Move -> XBoardMove
moveToXBM m =
  Algebraic (start, end) Nothing
  where
    start = head (pickUp m)
    end = (piecePos . snd . head) (putDown m)

setParseState :: XBoardParseState -> EngineM ()
setParseState x = do
  modify (\s -> s {parseState = x})

getParseState :: EngineM (XBoardParseState)
getParseState = do
  parseState <$> get

sendCommand :: E2X.EngineToX -> EngineM ()
sendCommand cmd =
  do
    let cmdText = E2X.commandText cmd
    debugPrintLn $ "sending " <> cmdText
    liftIO $
      TIO.putStrLn
        cmdText

xBoardLoop :: EngineM (ExitCode)
xBoardLoop =
  let handleCommand subRoutine = do
        rawLine <- liftIO getLogLine
        let cmd = parseLine rawLine
        debugPrintLn (T.show cmd)
        case parseLine rawLine of
          Just X2E.Quit -> do
            setParseState Quitting
          Just (X2E.Ping n) -> do
            sendCommand (E2X.Pong n)
          x -> subRoutine x
        xBoardLoop
   in do
        status <- getParseState
        case status of
          ExpectXBoard -> do
            handleCommand $ \_ -> do
              setParseState ExpectProtoVer
          ExpectProtoVer -> do
            handleCommand $ \protoVerLine -> do
              case protoVerLine of
                Just (X2E.Protover _) ->
                  setParseState DumpFeatures
                _ -> setParseState ParseError
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
                    E2X.SigTerm False,
                    E2X.PingF True
                  ]
              )
            liftIO $ TIO.putStrLn "feature myname=\"Shakuni\" memory=0 smp=0"
            sendCommand (E2X.Feature [E2X.Done True])
            setParseState Ready
            xBoardLoop
          ParseError -> do
            return $ ExitFailure 1
          Ready -> do
            handleCommand $ \cmd -> do
              -- debugPrintLn (T.show cmd)
              case cmd of
                Just X2E.New -> do
                  setParseState (InGame initState)
                _ -> return ()
          InGame gs -> do
            handleCommand $ \cmd -> do
              case cmd of
                Just (X2E.UserMove _) -> do
                  let currentState = makeNullMove gs
                  setParseState (InGame $ currentState)
                  debugPrintLn (T.show currentState)
                  let moves = availableMoves currentState

                  debugPrintLn (T.show moves)
                  case moves of
                    [] -> return ()
                    (x : _) -> do
                      setParseState (InGame (makeMove currentState x))
                      liftIO $ threadDelay 1000000
                      liftIO $ TIO.putStrLn (T.unwords ["1", "0", "0", "0", (T.show $ moveToXBM x)])
                      liftIO $ threadDelay 1000000
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
  (exitCode, _) <- runStateT xBoardMain (EngineState {parseState = ExpectXBoard, shouldPonder = False})
  exitWith exitCode

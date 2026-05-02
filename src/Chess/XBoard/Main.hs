module Chess.XBoard.Main (main) where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (LineBuffering), stdout)

data EngineState
  = ExpectXBoard
  | ExpectProtoVer
  | ParseError
  | Quitting
  deriving (Show)

getLogLine :: IO T.Text
getLogLine = do
  line <- TIO.getLine
  -- hPutStr
  --   stderr
  --   "(input) "
  -- hPutStr stderr line
  return line

type EngineM = StateT EngineState IO

xBoardLoop :: EngineM (ExitCode)
xBoardLoop =
  let handleCommand subRoutine = do
        line <- liftIO getLogLine
        case line of
          "quit" -> do
            put Quitting
          x -> subRoutine x
        xBoardLoop
   in do
        status <- get
        case status of
          ExpectXBoard -> do
            handleCommand $ \_ -> do
              put ExpectProtoVer
          ExpectProtoVer -> do
            handleCommand $ \protoVerLine -> do
              let ws = T.words protoVerLine
              case ws of
                ["protover", version] -> do
                  liftIO $ TIO.putStrLn (T.show version)
                _ -> do
                  liftIO $ TIO.putStrLn "Didn't understand"
          Quitting -> do
            return ExitSuccess
          _ -> do
            liftIO $ TIO.putStrLn ("Unhandled state " <> (T.show status))
            return $ ExitFailure 1

xBoardMain :: EngineM (ExitCode)
xBoardMain = do
  xBoardLoop

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (exitCode, _) <- runStateT xBoardMain (ExpectXBoard)
  exitWith exitCode

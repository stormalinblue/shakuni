module Chess.XBoard.Main (main) where

import Control.Monad.State
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Exit (ExitCode (..), exitWith)
import System.IO (Newline, hPutStr, stderr)

data EngineState
  = UnInit
  | Init
  | ParseError

data XToEngine
  = XBoard
  | New
  | Level Int Int Int
  | Post
  | Hard

getLogLine :: IO String
getLogLine = do
  line <- getLine
  -- hPutStr
  --   stderr
  --   "(input) "
  -- hPutStr stderr line
  return line

type EngineM = StateT EngineState IO

xBoardLoop :: EngineM (ExitCode)
xBoardLoop = do
  xBoard <- liftIO getLogLine
  put Init
  liftIO $ putStrLn "Acknowledged!"
  return (ExitSuccess)

xBoardMain :: EngineM (ExitCode)
xBoardMain = do
  xBoardLoop

main :: IO ()
main = do
  (exitCode, _) <- runStateT xBoardMain (UnInit)
  exitWith exitCode

{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module XDoToolWrapper 
( W(..)
, sendKey
, Key(..)
, runW
) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified System.Process  as P
import System.Exit (ExitCode(..))

data Key = Key Char

type Windows = M.Map String Int

data WState = WState
  { windows :: !Windows -- ^ Open windows
  }

type W a = StateT WState IO a

emptyState :: WState
emptyState = WState M.empty

runW :: W a -> IO a
runW w = evalStateT w emptyState

safeXDoTool :: [String] -> W ()
safeXDoTool args = do
  (exitcode, stdout, stderr) <- liftIO $ P.readProcessWithExitCode "xdotool" args ""
  printError exitcode stderr
    where
      printError ExitSuccess _ = return ()
      printError _ stderr      = liftIO $ print stderr

sendKey :: Key -> W ()
sendKey (Key k) = safeXDoTool ["key", [k]]

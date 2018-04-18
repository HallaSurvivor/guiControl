module XDoToolWrapper 
  ( Key(..)
  , sendKey
  ) where

import Core
import Control.Monad.IO.Class (liftIO) 
import System.Exit      (ExitCode(..))
import System.Process (readProcessWithExitCode)

data Key = Key Char

safeXDoTool :: [String] -> R ()
safeXDoTool args = do
  (exitcode, stdout, stderr) <- liftIO $ readProcessWithExitCode "xdotool" args ""
  liftIO $ printError exitcode stderr
    where
      printError ExitSuccess _ = return ()
      printError _ stderr      = print stderr

sendKey :: Key -> R ()
sendKey (Key k) = safeXDoTool ["key", [k]]

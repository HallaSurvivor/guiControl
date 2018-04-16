{-# LANGUAGE LambdaCase #-}
module XDoToolWrapper 
( sendKey
, Key(..)
) where

import Core
import Control.Monad.State.Strict
import qualified System.Process  as P
import System.Exit (ExitCode(..))

data Key = Key Char

safeXDoTool :: [String] -> R ()
safeXDoTool args = do
  (exitcode, stdout, stderr) <- liftIO $ P.readProcessWithExitCode "xdotool" args ""
  printError exitcode stderr
    where
      printError ExitSuccess _ = return ()
      printError _ stderr      = liftIO $ print stderr

sendKey :: Key -> R ()
sendKey (Key k) = safeXDoTool ["key", [k]]

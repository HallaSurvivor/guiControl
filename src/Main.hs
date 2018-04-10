module Main where

import XDoToolWrapper
import System.Console.ANSI
import System.IO
import Control.Monad.Trans

loop :: W ()
loop = do
  k <- liftIO $ getChar
  sendKey (Key k)
  loop

main :: IO ()
main = do
  initialize
  runW loop
  exit

    where
      initialize = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hideCursor

      exit = do
        clearScreen
        setCursorPosition 0 0
        showCursor

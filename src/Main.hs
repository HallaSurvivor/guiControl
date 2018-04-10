module Main where

import XDoToolWrapper
import System.Console.ANSI
import System.IO
import Control.Monad.Trans

terminalOff :: IO ()
terminalOff = do
  clearScreen
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor

terminalOn :: IO ()
terminalOn = do
  hSetEcho stdin True
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  clearScreen
  setCursorPosition 0 0
  showCursor


normalMode :: W ()
normalMode = do
  liftIO $ terminalOff
  loop

commandMode :: W ()
commandMode = do
  liftIO $ putStr ":"
  liftIO $ terminalOn
  cmd <- liftIO $ getLine
  runCmd cmd

runCmd :: String -> W ()
runCmd cmd = case cmd of
  "q"    -> return ()
  "quit" -> return ()
  _      -> normalMode

loop :: W ()
loop = do
  k <- liftIO $ getChar
  case k of
    ':' -> commandMode
    _   -> sendKey (Key k) >> loop

main :: IO ()
main = runW normalMode

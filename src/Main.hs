module Main where

import Core
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


normalMode :: R ()
normalMode = do
  liftIO terminalOff
  loop

commandMode :: R ()
commandMode = do
  liftIO $ putStr ":"
  liftIO terminalOn
  cmd <- liftIO getLine
  runCmd cmd

-- TODO: allow window switching, opening new windows
runCmd :: String -> R ()
runCmd cmd = case cmd of
  "q"    -> return ()
  "quit" -> return ()
  _      -> normalMode

loop :: R ()
loop = do
  k <- liftIO getChar
  case k of
    ':' -> commandMode
    _   -> sendKey (Key k) >> loop

main :: IO ()
main = runR defaultConfig startState normalMode >> return ()

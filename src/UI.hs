module UI 
  ( drawMode
  , setNoBuffering
  , setBuffering
  , setNoCursor
  , setCursor
  ) where

import Core
import System.Console.ANSI
import System.IO
import Control.Monad.State

drawMode :: R ()
drawMode = do
  _mode <- mode <$> get
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  liftIO $ print _mode

setNoBuffering :: R ()
setNoBuffering = liftIO $ do
  hSetEcho      stdin False
  hSetBuffering stdin NoBuffering

setBuffering :: R ()
setBuffering = liftIO $ do
  hSetEcho      stdin True
  hSetBuffering stdin LineBuffering

setNoCursor :: R ()
setNoCursor = liftIO hideCursor

setCursor :: R ()
setCursor = liftIO showCursor

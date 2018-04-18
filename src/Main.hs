module Main where

import Core
import DefaultConfig
import XDoToolWrapper
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Maybe

initialize :: R ()
initialize = do
  join $ toNormal <$> ask
  loop

-- | The main loop
loop :: R ()
loop = do
  _mode <- mode <$> get
  case _mode of
    Command -> do
      _cmdNotFound <- cmdNotFound <$> ask
      cmd <- liftIO getLine
      runCommand cmd `catchError` _cmdNotFound
      mode' <- mode <$> get
      unless (mode' == Quitting) (join $ toNormal <$> ask)
      loop
    Normal -> do 
      k <- liftIO getChar
      runCommand [k] `catchError` (\_ -> return ())
      loop
    Insert -> do
      k <- liftIO getChar
      runCommand [k] `catchError` (\_ -> sendKey (Key k))
      loop
    Quitting -> return ()


main :: IO ()
main = runR defaultConfig startState initialize >> return ()

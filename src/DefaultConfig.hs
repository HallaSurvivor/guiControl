module DefaultConfig 
  ( defaultConfig
  ) where

import Core
import UI
import Control.Monad.State.Strict (modify', liftIO)
import qualified Data.Map.Strict as M

_toQuitting :: R ()
_toQuitting = do
  modify' (\st -> st { mode = Quitting })
  setBuffering
  setCursor


_toCommand :: R ()
_toCommand = do
  modify' (\st -> st { mode = Command })
  drawMode
  setBuffering
  setCursor
  liftIO $ putStr ":"

_toNormal :: R ()
_toNormal = do
  modify' (\st -> st { mode = Normal })
  drawMode
  setNoBuffering
  setNoCursor

_toInsert :: R ()
_toInsert = do
  modify' (\st -> st { mode = Insert })
  drawMode
  setNoBuffering
  setNoCursor

_cmdNotFound :: String -> R ()
_cmdNotFound cmd = liftIO $ putStrLn $ "Command ``" ++ cmd ++ "'' not found"

-- | Default configuration
defaultConfig :: WConfig
defaultConfig = WConfig 
  { commands    = _commands
  , toNormal    = _toNormal
  , toInsert    = _toInsert
  , toCommand   = _toCommand
  , toQuitting  = _toQuitting
  , cmdNotFound = _cmdNotFound
  }
  where
    normalCommands =
      [ (":", _toCommand)
      , ("i", _toInsert)
      ]

    insertCommands =
      [ ("q", _toNormal) 
      ]

    commandCommands = 
      [ ("q"   , _toQuitting)
      , ("quit", _toQuitting)
      ]

    _commands = M.fromList $
      fmap (\(str, cmd) -> ((Normal,  str), cmd)) normalCommands ++
      fmap (\(str, cmd) -> ((Insert,  str), cmd)) insertCommands ++
      fmap (\(str, cmd) -> ((Command, str), cmd)) commandCommands

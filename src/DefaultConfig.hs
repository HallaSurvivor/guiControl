module DefaultConfig 
  ( defaultConfig
  ) where

import Core
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

_toQuitting :: R ()
_toQuitting = do
  terminalOn
  liftIO $ putStrLn "quitting..."
  modify' (\st -> st { mode = Quitting })

_toCommand :: R ()
_toCommand = do
  terminalOn
  liftIO $ putStr ":"
  modify' (\st -> st { mode = Command })

_toNormal :: R ()
_toNormal = do
  terminalOff
  modify' (\st -> st { mode = Normal })

_toInsert :: R ()
_toInsert = do
  terminalOff
  modify' (\st -> st { mode = Insert })

_cmdNotFound :: String -> R ()
_cmdNotFound cmd = liftIO $ putStrLn $ "Command ``" ++ cmd ++ "'' not found"

_drawStatus :: R()
_drawStatus = do
  mode <- mode <$> get
  liftIO $ print mode

-- | Default configuration
defaultConfig :: WConfig
defaultConfig = WConfig 
  { commands    = _commands
  , toNormal    = _toNormal
  , toInsert    = _toInsert
  , toCommand   = _toCommand
  , toQuitting  = _toQuitting
  , cmdNotFound = _cmdNotFound
  , drawStatus = _drawStatus
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

module DefaultConfig 
  ( defaultConfig
  ) where

import Core
import XDoToolWrapper
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
_cmdNotFound cmd = do 
  liftIO $ putStrLn $ "Command ``" ++ cmd ++ "'' not found"
  _toNormal

_ctrlF :: R ()
_ctrlF = sendKey (Ctrl (Key 'f')) >> _toInsert

_switchFocus :: R ()
_switchFocus = undefined

_sendXDoToolCommand :: R ()
_sendXDoToolCommand = undefined

_useMouse :: R ()
_useMouse = undefined

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
      [ ("`", _toNormal) 
      ]

    commandCommands = 
      [ ("q"   , _toQuitting)
      , ("quit", _toQuitting)
      , ("find", _ctrlF)
      ]

    _commands = M.fromList $
      fmap (\(str, cmd) -> ((Normal,  str), cmd)) normalCommands ++
      fmap (\(str, cmd) -> ((Insert,  str), cmd)) insertCommands ++
      fmap (\(str, cmd) -> ((Command, str), cmd)) commandCommands

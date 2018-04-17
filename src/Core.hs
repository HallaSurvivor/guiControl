{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core
( R(..)
, runR
, startState
, defaultConfig
) where

import Control.Monad.State.Strict
import Control.Monad.Reader

import qualified Data.Map.Strict as M

type Windows   = M.Map String Int -- ^ Window name, Window ID

-- | Modes that we can be in, and the commands available in each mode
data Mode = Normal | Insert | Command
  deriving (Eq, Ord)

type Commands = M.Map (Mode, String) (R ()) -- ^ Mode/Command name to Command

data WState = WState
  { windows :: !Windows -- ^ Open windows
  , mode    :: !Mode    -- ^ Current execution mode
  }

data WConfig = WConfig
  { commands :: !Commands
  }

-- | R has read access to WConfig and read/write access to WState
newtype R a = R (ReaderT WConfig (StateT WState IO) a)
  deriving (Functor, Monad, MonadIO, MonadState WState, MonadReader WConfig)

instance Applicative R where
  pure  = return
  (<*>) = ap


-- | Execute a block of R code given a config and start state
runR :: WConfig -> WState -> R a -> IO (a, WState)
runR cfg st (R x) = runStateT (runReaderT x cfg) st

-- | Starting State
startState :: WState
startState = WState M.empty Normal

-- | Modify an existing command in a given mode (adds it if it doesn't exist)
modifyCommand :: WConfig -> Mode -> String -> R () -> WConfig
modifyCommand cfg mode name cmd = cfg { commands = commands' }
  where
    commands' = M.insert (mode, name) cmd (commands cfg)

quit :: R ()
quit = return ()

toNormal :: R ()
toNormal = do
  st <- get
  put $ st { mode = Normal }

toCommand :: R ()
toCommand = do
  st <- get
  put $ st { mode = Command }

-- | Default configuration
defaultConfig :: WConfig
defaultConfig = WConfig commands
  where
    normalCommands =
      [ (":", toCommand)
      ]

    insertCommands =
      [ ("<esc>", toNormal) 
      ]

    commandCommands = 
      [ ("q"   , quit)
      , ("quit", quit)
      ]

    commands = M.fromList $
      fmap (\(str, cmd) -> ((Normal,  str), cmd)) normalCommands ++
      fmap (\(str, cmd) -> ((Insert,  str), cmd)) insertCommands ++
      fmap (\(str, cmd) -> ((Command, str), cmd)) commandCommands

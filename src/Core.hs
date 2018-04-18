{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core
  ( R(..)
  , WConfig(..)
  , WState(..)
  , Mode(..)
  , runR
  , startState
  , runCommand
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map.Strict as M

type Windows   = M.Map String Int -- ^ Window name, Window ID

-- | Modes that we can be in, and the commands available in each mode
data Mode = Normal | Insert | Command | Quitting
  deriving (Eq, Ord, Show)

type Commands = M.Map (Mode, String) (R ()) -- ^ Mode/Command name to Command

data WState = WState
  { windows :: !Windows -- ^ Open windows
  , mode    :: !Mode    -- ^ Current execution mode
  }

data WConfig = WConfig
  { commands    :: !Commands
  , toNormal    :: R ()
  , toInsert    :: R ()
  , toCommand   :: R ()
  , toQuitting  :: R ()
  , cmdNotFound :: String -> R ()
  }

-- | R has read access to WConfig and read/write access to WState
newtype R a = R (ReaderT WConfig (StateT WState (ExceptT String IO)) a)
  deriving (Functor, Monad, MonadIO, MonadState WState, MonadReader WConfig, MonadError String)

instance Applicative R where
  pure  = return
  (<*>) = ap


-- | Execute a block of R code given a config and start state
runR :: WConfig -> WState -> R a -> IO (Either String (a, WState))
runR cfg st (R x) = runExceptT $ runStateT (runReaderT x cfg) st

-- | Starting State
startState :: WState
startState = WState M.empty Normal

-- | Run the requested cmd, throw an error if it doesn't exist
runCommand :: String -> R ()
runCommand cmd = do
  _mode <- mode <$> get
  _cmds <- commands <$> ask
  fromMaybe (throwError cmd) (M.lookup (_mode, cmd) _cmds)


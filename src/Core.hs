{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core
  ( R(..)
  , WConfig(..)
  , WState(..)
  , Mode(..)
  , Key(..)
  , runR
  , startState
  , runCommand
  , terminalOn
  , terminalOff
  , sendKey
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import System.Console.ANSI
import System.IO
import System.Exit (ExitCode(..))
import qualified Data.Map.Strict as M
import qualified System.Process as P

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
  , drawStatus :: R ()
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


data Key = Key Char

safeXDoTool :: [String] -> R ()
safeXDoTool args = do
  (exitcode, stdout, stderr) <- liftIO $ P.readProcessWithExitCode "xdotool" args ""
  liftIO $ printError exitcode stderr
    where
      printError ExitSuccess _ = return ()
      printError _ stderr      = print stderr

sendKey :: Key -> R ()
sendKey (Key k) = safeXDoTool ["key", [k]]

terminalOff :: R ()
terminalOff = liftIO $ do
  clearScreen
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor

terminalOn :: R ()
terminalOn = liftIO $ do
  hSetEcho stdin True
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  clearScreen
  setCursorPosition 0 0
  showCursor

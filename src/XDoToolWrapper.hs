{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XDoToolWrapper 
(
) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import HSH

data Key = Key Char

newtype Windows = M.Map String Int

data WState = WState
  { windows :: !Windows -- ^ Open windows
  }

newtype W a = W (StateT WState IO a)
  deriving (Functor, Monad, MonadIO, MonadState WState)

instance Applicative W where
  pure = return
  (<*>) = ap


checkInstalled :: IO Bool
checkInstalled = run "hash xdotool"

sendKey :: Key -> IO ()
sendKey (Key k) = run $ "xdotool key " ++ [k]



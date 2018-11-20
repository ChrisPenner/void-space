module Actions.Game where

import           Control.Monad.State
import           Control.Concurrent
import           Types
import           Control.Lens

speedUp :: (HasGameState s, MonadIO m, MonadState s m) => m ()
speedUp = do
  spd <- use score
  void (use speedVar >>= liftIO . (`swapMVar` spd))

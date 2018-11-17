{-# LANGUAGE FlexibleContexts #-}
module Actions.Actions where

import Control.Monad.State
import Control.Lens
import Data.GameState
import Actions.EnemyActions

tick :: (MonadState (GameState n) m, MonadIO m) => m ()
tick = do
  ticks += 1
  stepEnemies
  spawnEnemies

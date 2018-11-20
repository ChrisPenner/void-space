module Actions.Health where

import Control.Lens
import Control.Monad.State
import Types
import Config

hurtBy :: (HasHealth s, MonadState s m) => Float -> m ()
hurtBy amt = do
  hasShields <- uses shields (> 0)
  if hasShields
    then shields %= max 0 . subtract amt
    else hp %= max 0 . subtract amt

incTimeSinceHit :: (HasHealth s, MonadState s m) => m ()
incTimeSinceHit = do
  v <- timeSinceHit <+= 1
  when (v > shieldRecoveryTime) (shields %= min 1 . (+ shieldRecoveryAmount))

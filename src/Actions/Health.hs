module Actions.Health where
import Control.Monad.State
import Data.Health
import Control.Lens

hurtBy :: (HasHealth s, MonadState s m) => Float -> m ()
hurtBy amt = do
  hasShields <- uses shields (> 0)
  if hasShields then shields -= amt else hp -= amt

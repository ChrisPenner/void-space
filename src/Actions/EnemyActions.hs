{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Actions.EnemyActions where

import qualified Data.Text as T
import Control.Monad.State
import Data.Enemies
import Data.GameState
import Data.Words
import Data.Ship
import Control.Lens as L
import Data.Monoid
import Data.Health
import Control.Applicative
import System.Random

stepEnemies :: (HasEnemies s n MEnemy, MonadState s m) => m ()
stepEnemies = enemies . traverse . _Just . distance -= 1

shouldSpawn :: (MonadIO m) => m Bool
shouldSpawn = (<= spawnPercentage) <$> liftIO (randomRIO (0, 1))
 where
  spawnPercentage :: Float
  spawnPercentage = 0.3

spawnEnemies
  :: ( MonadIO m
     , HasWordStream s
     , HasEnemies s n MEnemy
     , HasShip s
     , MonadState s m
     )
  => m ()
spawnEnemies = do
  shouldSpawn' <- shouldSpawn
  when shouldSpawn' newEnemy

newEnemy
  :: forall s n m
   . ( HasWordStream s
     , HasShip s
     , HasEnemies s n MEnemy
     , MonadState s m
     , MonadIO m
     )
  => m ()
newEnemy = do
  sz  <- corridorSize
  loc <- liftIO $ randomRIO (0, sz)
  w   <- getWord
  enemies . traversed . L.index loc %= addIfMissing loc w
 where
  addIfMissing loc w e =
    e <|> Just Enemy {_row = loc, _distance = 50, _word = Left w}

checkDamage
  :: forall s n m . (HasHealth s, HasEnemies s n MEnemy, MonadState s m) => m ()
checkDamage = do
  Sum totalDamagingEnemies <-
    enemies . traversed %%= \x -> if has (_Just . distance . filtered (<= 0)) x
      then (Sum 1, Nothing)
      else (Sum 0, x)
  health . hp -= (fromIntegral totalDamagingEnemies * 0.1)

killEnemies :: forall s n m . (HasEnemies s n MEnemy, MonadState s m) => m ()
killEnemies = (enemies . traversed) %= maybeKill
 where
  maybeKill e =
    if has (_Just . word . _Left) e
         || has (_Just . word . _Right . untyped . to (not . T.null)) e
      then e
      else Nothing

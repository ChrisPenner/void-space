{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Actions.Enemy where

import qualified Data.Text as T
import Control.Monad.State
import Data.Enemies
import Data.Words
import Data.Ship
import Control.Lens as L
import Data.Monoid
import Data.Health
import Control.Applicative
import System.Random
import Actions.Words
import Actions.Health
import Data.GameState

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
      then (Sum 1 :: Sum Int, Nothing)
      else (Sum 0, x)
  hurtBy (fromIntegral totalDamagingEnemies * 0.3)

killEnemies
  :: forall s n m
   . (HasGameState s n, HasEnemies s n MEnemy, MonadState s m)
  => m ()
killEnemies = do
  Sum numKilled <- (enemies . traversed) %%= maybeKill
  score += numKilled
 where
  maybeKill Nothing = (Sum 0, Nothing)
  maybeKill (Just e) =
    if has (word . _Left) e
         || has (word . _Right . untyped . filtered (not . T.null)) e
      then (Sum 0, Just e)
      else (Sum 1, Nothing)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Actions.Enemy where

import Control.Monad.State
import Data.Enemies
import Data.Words
import Data.Art
import Control.Lens as L
import Data.Monoid
import Data.Health
import Control.Applicative
import System.Random
import Actions.Words
import Actions.Health
import Data.GameState

stepEnemies :: (HasEnemies s n (Maybe Enemy), MonadState s m) => m ()
stepEnemies = enemies . traverse . _Just . distance -= 1

shouldSpawn :: (MonadIO m) => m Bool
shouldSpawn = (<= spawnPercentage) <$> liftIO (randomRIO (0, 1))
 where
  spawnPercentage :: Float
  spawnPercentage = 0.3

spawnEnemies
  :: ( MonadIO m
     , HasWordStream s
     , HasEnemies s n (Maybe Enemy)
     , HasArt s
     , MonadState s m
     )
  => m ()
spawnEnemies = do
  shouldSpawn' <- shouldSpawn
  when shouldSpawn' newEnemy

newEnemy
  :: forall s n m
   . ( HasWordStream s
     , HasArt s
     , HasEnemies s n (Maybe Enemy)
     , MonadState s m
     , MonadIO m
     )
  => m ()
newEnemy = do
  sz  <- corridorSize
  loc <- liftIO $ randomRIO (0, sz)
  w   <- getWord
  let newWord = WordT {_untyped = w, _typed = ""}
  enemies . traversed . L.index loc %= addIfMissing loc newWord
 where
  addIfMissing loc w e =
    e <|> Just Enemy {_row = loc, _distance = 50, _word = w}

checkDamage
  :: forall s n m
   . (HasHealth s, HasEnemies s n (Maybe Enemy), MonadState s m)
  => m ()
checkDamage = do
  Sum totalDamagingEnemies <-
    enemies . traversed %%= \x -> if has (_Just . distance . filtered (<= 0)) x
      then (Sum 1 :: Sum Int, Nothing)
      else (Sum 0, x)
  hurtBy (fromIntegral totalDamagingEnemies * 0.3)
  when (totalDamagingEnemies > 0) $ timeSinceHit .= 0

killEnemies
  :: forall s n m
   . (HasGameState s n, HasEnemies s n (Maybe Enemy), MonadState s m)
  => m ()
killEnemies = do
  Sum numKilled <- (enemies . traversed) %%= maybeKill
  score += numKilled
 where
  maybeKill (Just (view (word . untyped) -> "")) = (Sum 1, Nothing)
  maybeKill e = (Sum 0, e)

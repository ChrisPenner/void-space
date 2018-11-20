{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Actions.Enemy where

import           Actions.Health
import           Actions.Words
import           Config
import           Control.Applicative
import           Control.Lens                  as L
import           Control.Monad.State
import           Data.Monoid
import           System.Random
import           Types
import           Actions.Game
import qualified Data.Text                     as T


corridorSize :: (HasArt s, MonadState s m) => m Int
corridorSize = uses ship (length . T.lines)

stepEnemies :: (HasEnemies s, MonadState s m) => m ()
stepEnemies = enemies . _Just . distance -= 1

shouldSpawn :: (MonadIO m) => m Bool
shouldSpawn = (<= spawnPercentage) <$> liftIO (randomRIO (0, 1))

spawnEnemies
  :: (MonadIO m, HasWordStream s, HasEnemies s, HasArt s, MonadState s m)
  => m ()
spawnEnemies = do
  shouldSpawn' <- shouldSpawn
  when shouldSpawn' newEnemy

newEnemy
  :: (HasWordStream s, HasArt s, HasEnemies s, MonadState s m, MonadIO m)
  => m ()
newEnemy = do
  sz  <- corridorSize
  loc <- liftIO $ randomRIO (0, sz)
  w   <- getWord
  let newWord = WordT {_untyped = w, _typed = ""}
  enemies . L.index loc %= addIfMissing loc newWord
 where
  addIfMissing loc w e =
    e <|> Just Enemy {_row = loc, _distance = corridorWidth, _word = w}

checkDamage :: (HasHealth s, HasEnemies s, MonadState s m) => m ()
checkDamage = do
  Sum totalDamagingEnemies <-
    enemies %%= \x -> if has (_Just . distance . filtered (<= 0)) x
      then (Sum 1 :: Sum Int, Nothing)
      else (Sum 0, x)
  hurtBy (fromIntegral totalDamagingEnemies * enemyDamage)
  when (totalDamagingEnemies > 0) $ timeSinceHit .= 0

killEnemies :: (MonadIO m, HasGameState s, HasEnemies s, MonadState s m) => m ()
killEnemies = do
  Sum numKilled <- enemies %%= maybeKill
  score += numKilled
  when (numKilled > 0) speedUp
 where
  maybeKill (Just (view (word . untyped) -> "")) = (Sum 1, Nothing)
  maybeKill e = (Sum 0, e)

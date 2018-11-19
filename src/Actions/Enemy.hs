{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Actions.Enemy where

import           Control.Monad.State
import           Control.Lens                  as L
import           Data.Monoid
import           Control.Applicative
import           System.Random
import           Actions.Words
import           Actions.Health
import           Types
import qualified Data.Text as T

corridorSize :: (HasArt s, MonadState s m) => m Int
corridorSize = uses ship (length . T.lines)

stepEnemies :: (HasEnemies s, MonadState s m) => m ()
stepEnemies = enemies . _Just . distance -= 1

shouldSpawn :: (MonadIO m) => m Bool
shouldSpawn = (<= spawnPercentage) <$> liftIO (randomRIO (0, 1))
 where
  spawnPercentage :: Float
  spawnPercentage = 0.3

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
    e <|> Just Enemy {_row = loc, _distance = 50, _word = w}

checkDamage :: (HasHealth s, HasEnemies s, MonadState s m) => m ()
checkDamage = do
  Sum totalDamagingEnemies <-
    enemies %%= \x -> if has (_Just . distance . filtered (<= 0)) x
      then (Sum 1 :: Sum Int, Nothing)
      else (Sum 0, x)
  hurtBy (fromIntegral totalDamagingEnemies * 0.3)
  when (totalDamagingEnemies > 0) $ timeSinceHit .= 0

killEnemies
  :: forall s n m . (HasGameState s n, HasEnemies s, MonadState s m) => m ()
killEnemies = do
  Sum numKilled <- enemies %%= maybeKill
  score += numKilled
 where
  maybeKill (Just (view (word . untyped) -> "")) = (Sum 1, Nothing)
  maybeKill e = (Sum 0, e)

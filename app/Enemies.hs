{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Enemies where

import           Control.Lens
import           Brick
import           Brick.Markup
import           Control.Monad.State
import qualified Data.Text                     as T
import           Data.Functor.Compose
import           Data.Functor.Selection
import           Control.Lens.Selection
import           Data.Coerce
import           Control.Monad.Supply
import           Attrs
import           Words
import           System.Random
import           Ship

data Enemy a = Enemy
  { _distance :: Int
  , _row :: Int
  , _word :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Enemy

newtype EnemyState = EnemyState
  { _enemies :: [Enemy (Either T.Text FocusedWord)]
  }

makeClassy ''EnemyState

instance HasWords EnemyState where
  eachWord = enemies . traversed . word

enemiesStart :: EnemyState
enemiesStart = EnemyState []

stepEnemies :: (HasEnemyState s, MonadState s m) => m ()
stepEnemies = enemies . traverse . distance -= 1

shouldSpawn :: (MonadIO m) => m Bool
shouldSpawn = (<= spawnPercentage) <$> liftIO (randomRIO (0, 1))
 where
  spawnPercentage :: Float
  spawnPercentage = 0.3

spawnEnemies
  :: (MonadIO m, HasWordStream s, HasEnemyState s, HasShip s, MonadState s m)
  => m ()
spawnEnemies = do
  shouldSpawn' <- shouldSpawn
  when shouldSpawn' newEnemy

newEnemy
  :: (HasWordStream s, HasShip s, HasEnemyState s, MonadState s m, MonadIO m)
  => m ()
newEnemy = do
  sz  <- corridorSize
  loc <- liftIO $ randomRIO (0, sz)
  w   <- getWord
  enemies <>= [Enemy {_row = loc, _distance = 50, _word = Left w}]


killEnemies :: forall m s . (HasEnemyState s, MonadState s m) => m ()
killEnemies = enemies %= toListOf (traversed . filtered alive)
 where
  alive :: Enemy WordT -> Bool
  alive =
    has $ word . choosing united (untyped . filtered (not . T.null) . united)

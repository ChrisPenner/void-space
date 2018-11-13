{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module GameState where

import           Control.Lens
import qualified Data.Text                     as T
import           Enemies
import           Data.Stream.Infinite          as S
import           Words
import           Control.Monad.State
import           Ship


data GameState = GameState
  { _enemiesState :: EnemyState
  , _shipState :: Ship
  , _wordStream' :: S.Stream T.Text
  , _ticks :: Int
  }

makeClassy ''GameState

instance HasWords GameState where
  eachWord = enemiesState . eachWord

instance HasWordStream GameState where
  wordStream = wordStream'

instance HasEnemyState GameState where
  enemyState = enemiesState

instance HasShip GameState where
  ship = shipState


gameStart :: S.Stream T.Text -> Ship -> GameState
gameStart (S.splitAt 5 -> (startWords, aWordStream)) aShip = GameState
  { _enemiesState = enemiesStart
  , _shipState    = aShip
  , _wordStream'  = aWordStream
  , _ticks        = 0
  }

tick :: (MonadState GameState m, MonadIO m) => m ()
tick = do
  ticks += 1
  stepEnemies
  spawnEnemies

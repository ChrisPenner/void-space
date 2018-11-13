{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module GameState where

import           Control.Lens
import qualified Data.Text                     as T
import           Enemies
import           Data.Stream.Infinite          as S
import           Words
import           Control.Monad.State
import           Ship
import           GHC.TypeLits


data GameState n where
  GameState  :: KnownNat n => {
      _enemiesState :: Enemies n MEnemy
    , _shipState :: Ship
    , _wordStream' :: S.Stream T.Text
    , _ticks :: Int
    } -> GameState n

makeClassy ''GameState

instance HasWords (GameState n) where
  eachWord = enemies . traversed . _Just .  eachWord

instance HasWordStream (GameState n) where
  wordStream = wordStream'

instance HasEnemies (GameState n) n MEnemy where
  enemies = enemiesState

instance HasShip (GameState n) where
  ship = shipState


gameStart :: S.Stream T.Text -> Ship -> GameState 5
gameStart (S.splitAt 5 -> (startWords, aWordStream)) aShip = GameState
  { _enemiesState = enemiesStart
  , _shipState    = aShip
  , _wordStream'  = aWordStream
  , _ticks        = 0
  }

tick :: (MonadState (GameState n) m, MonadIO m) => m ()
tick = do
  ticks += 1
  stepEnemies
  spawnEnemies

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
module Data.GameState where

import           Control.Lens
import qualified Data.Text                     as T
import           Data.Stream.Infinite          as S
import           Data.Words
import           Data.Ship
import           GHC.TypeLits
import           Data.Health
import           Data.Enemies

data GameState n where
  GameState  :: KnownNat n => {
      _enemiesState :: Enemies n MEnemy
    , _shipState :: Ship
    , _wordStream' :: S.Stream T.Text
    , _healthState :: Health
    , _score :: Int
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

instance HasHealth (GameState n) where
  health = healthState

gameStart :: S.Stream T.Text -> Ship -> GameState 5
gameStart aWordStream aShip = GameState
  { _enemiesState = enemiesStart
  , _shipState    = aShip
  , _wordStream'  = aWordStream
  , _healthState  = startHealth
  , _score        = 20
  }

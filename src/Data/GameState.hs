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
import           Data.Art
import           GHC.TypeLits
import           Data.Health
import           Data.Enemies

data GameState n where
  GameState  :: KnownNat n => {
      _enemiesState :: Enemies n MEnemy
    , _artState :: Art
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

instance HasArt (GameState n) where
  art = artState

instance HasHealth (GameState n) where
  health = healthState

gameStart :: S.Stream T.Text -> Art -> GameState 5
gameStart aWordStream art' = GameState
  { _enemiesState = enemiesStart
  , _artState     = art'
  , _wordStream'  = aWordStream
  , _healthState  = startHealth
  , _score        = 0
  }

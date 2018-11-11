{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Enemies where

import Control.Lens
import Brick
import           Brick.Markup
import Control.Monad.State
import qualified Data.Text as T
import Data.Functor.Compose
import Data.Functor.Selection
import Control.Lens.Selection
import Data.Coerce
import Control.Monad.Supply
import           Attrs
import Words

data Enemy a = Enemy
  {_distance :: Int
  , _word :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Enemy

data EnemyState = EnemyState
  { _enemies :: [Enemy (Either T.Text FocusedWord)]
  }

makeClassy ''EnemyState

instance HasWords EnemyState where
  eachWord = enemies . traversed . word

enemiesStart :: EnemyState
enemiesStart = EnemyState
  [ Enemy 15 (Left "hi")
  , Enemy 20 (Left "hello")
  , Enemy 30 (Left "hello")
  , Enemy 40 (Left "hello")
  , Enemy 50 (Left "hello")
  ]

stepEnemies :: (HasEnemyState s, MonadState s m) => m ()
stepEnemies = do
  enemies . traverse . distance -= 1

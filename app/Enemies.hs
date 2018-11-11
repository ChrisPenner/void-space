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

newtype EnemyState = EnemyState
  { _enemies :: [Enemy (Either T.Text FocusedWord)]
  }

makeClassy ''EnemyState

instance HasWords EnemyState where
  eachWord = enemies . traversed . word

enemiesStart :: [T.Text] -> EnemyState
enemiesStart startWords = EnemyState
  (startWords ^.. traversed . withIndex . to
    (\(i, w) -> Enemy ((i * 10) + 10) (Left w))
  )

stepEnemies :: (HasEnemyState s, MonadState s m) => m ()
stepEnemies = enemies . traverse . distance -= 1

killEnemies :: forall m s . (HasEnemyState s, MonadState s m) => m ()
killEnemies = enemies %= toListOf (traversed . filtered alive)
 where
  alive :: Enemy WordT -> Bool
  alive = has $ word . choosing id (untyped . filtered (not . T.null))

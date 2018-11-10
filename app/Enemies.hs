{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Enemies where

import Control.Lens
import Brick
import Control.Monad.State
import qualified Data.Text as T
import Data.Functor.Compose
import Data.Functor.Selection
import Control.Lens.Selection
import Data.Coerce
import Words

data Enemy a = Enemy
  {_distance :: Int
  , _word :: a
  } deriving (Functor, Foldable)

makeLenses ''Enemy

data Enemies = Enemies
  { _enemies :: Selection (Compose [] Enemy) T.Text FocusedWord
  }

makeLenses ''Enemies

enemiesStart :: Enemies
enemiesStart = Enemies . Selection $ Compose
  [ Enemy 15 (Left "hi")
  , Enemy 20 (Left "hello")
  , Enemy 30 (Left "hello")
  , Enemy 40 (Left "hello")
  , Enemy 50 (Left "hello")
  ]

stepEnemies :: State Enemies ()
stepEnemies = do
  enemies . unwrapping . _Wrapped' . traverse . distance -= 1

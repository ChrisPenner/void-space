{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Enemies where

import Control.Lens
import Brick
import Control.Monad.State
import qualified Data.Text as T

data Enemy = Enemy
  {_distance :: Int
  , _word :: T.Text
  }

makeLenses ''Enemy

data Enemies = Enemies
  { _enemies :: [Enemy]
  }

makeLenses ''Enemies

enemiesStart :: Enemies
enemiesStart = Enemies
  [ Enemy 15 "hi"
  , Enemy 20 "hello"
  , Enemy 30 "hello"
  , Enemy 40 "hello"
  , Enemy 50 "hello"
  ]

stepEnemies :: State Enemies ()
stepEnemies = do
  enemies . traversed . distance -= 1

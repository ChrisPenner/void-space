{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module GameState where

import           Control.Lens
import qualified Data.Text                     as T
import           Enemies
import           Data.Stream.Infinite          as S

newtype Ship = Ship T.Text
data GameState = GameState { _enemiesState :: EnemyState, _ship :: Ship,  _wordStream :: S.Stream T.Text }

makeClassy ''GameState

instance HasEnemyState GameState where
  enemyState = enemiesState

gameStart :: S.Stream T.Text -> Ship -> GameState
gameStart wordStream' ship' = GameState
  { _enemiesState = enemiesStart
  , _ship         = ship'
  , _wordStream   = wordStream'
  }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module GameState where

import           Control.Lens
import qualified Data.Text                     as T
import           Enemies
import           Data.Stream.Infinite          as S
import           Words

newtype Ship = Ship T.Text
data GameState = GameState { _enemiesState :: EnemyState, _ship :: Ship,  _wordStream' :: S.Stream T.Text }

makeClassy ''GameState

instance HasWords GameState where
  eachWord = enemiesState . eachWord

instance HasWordStream GameState where
  wordStream = wordStream'

instance HasEnemyState GameState where
  enemyState = enemiesState

gameStart :: S.Stream T.Text -> Ship -> GameState
gameStart aWordStream aShip = GameState
  { _enemiesState = enemiesStart
  , _ship         = aShip
  , _wordStream'  = aWordStream
  }

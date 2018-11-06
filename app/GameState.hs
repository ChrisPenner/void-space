{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module GameState where

import           Control.Lens
import           Words
import qualified Data.Text                     as T
import           Data.List.NonEmpty
import qualified Data.Stream.Infinite          as S
import           Enemies

newtype Ship = Ship T.Text
data GameState = GameState { _wordState :: WordState, _enemiesState :: Enemies, _ship :: Ship }

makeLenses ''GameState

gameStart :: NonEmpty T.Text -> Ship -> GameState
gameStart (S.cycle -> wordList) ship' = GameState
  { _wordState    = wordStart wordList
  , _enemiesState = enemiesStart
  , _ship         = ship'
  }

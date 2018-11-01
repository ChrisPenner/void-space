{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Control.Lens
import Words
import qualified Data.Text as T
import Data.List.NonEmpty

data GameState = GameState { _wordState :: WordState }

makeLenses ''GameState

gameStart :: NonEmpty T.Text -> GameState
gameStart wordList = GameState {_wordState = wordsStart wordList}

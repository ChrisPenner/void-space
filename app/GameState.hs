{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module GameState where

import Control.Lens
import Words
import qualified Data.Text as T
import Data.List.NonEmpty
import qualified Data.Stream.Infinite          as S

data GameState = GameState { _wordState :: WordState }

makeLenses ''GameState

gameStart :: NonEmpty T.Text -> GameState
gameStart (S.cycle -> wordList) = GameState {_wordState = wordStart wordList}

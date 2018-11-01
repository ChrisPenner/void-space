{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Control.Lens
import Words

data GameState = GameState { _wordState :: WordState }

makeLenses ''GameState

gameStart :: GameState
gameStart = GameState {_wordState = WordState ("", "hippopotamus")}

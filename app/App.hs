{-# LANGUAGE OverloadedStrings #-}
module App where

import Brick
import Graphics.Vty.Attributes
import Data.Void
import Graphics.Vty.Input.Events
import Control.Monad.State

import Words
import GameState
import Dashboard
import Render
import Stars
import Attrs
import Brick.Widgets.Center
import Control.Lens
import Enemies

type ResourceName = Void
type CustomEvent = Void

app :: App GameState CustomEvent ResourceName
app = App
  { appDraw         = drawGame
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }


chooseCursor
  :: GameState
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor _ _ = Nothing

handleEvent
  :: GameState
  -> BrickEvent ResourceName CustomEvent
  -> EventM ResourceName (Next GameState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey (KChar c) _)) = continue $ flip execState s $ do
  zoom wordState    (typeKey c)
  zoom enemiesState stepEnemies
handleEvent s _ = continue s

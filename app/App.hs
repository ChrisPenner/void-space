{-# LANGUAGE OverloadedStrings #-}
module App where

import Brick
import Graphics.Vty.Attributes
import Data.Void

import GameState
import Dashboard
import Render
import Stars
import Attrs
import Brick.Widgets.Center

type ResourceName = Void
type CustomEvent = Void

app :: App GameState CustomEvent ResourceName
app = App
  { appDraw         = draw
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }


draw :: GameState -> [Widget n]
draw GameState = [stars, hCenter ship <=> dashboard]

chooseCursor
  :: GameState
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor _ _ = Nothing

handleEvent
  :: GameState
  -> BrickEvent ResourceName CustomEvent
  -> EventM ResourceName (Next GameState)
handleEvent s _ = halt s

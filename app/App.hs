{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import Brick
import Graphics.Vty.Attributes
import Data.Void
import Graphics.Vty.Input.Events
import Control.Monad.State
import Control.Monad.Supply

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
type CustomEvent = ()

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
handleEvent s (AppEvent ()) = continue (s &~ stepEnemies)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey (KChar c) _)) =
  continue . flip execState s $ typeChar c
handleEvent s _ = continue s

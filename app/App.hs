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

app :: App (GameState n) CustomEvent ResourceName
app = App
  { appDraw         = drawGame
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const attrs
  }


chooseCursor
  :: GameState n
  -> [CursorLocation ResourceName]
  -> Maybe (CursorLocation ResourceName)
chooseCursor _ _ = Nothing

handleEvent
  :: GameState n
  -> BrickEvent ResourceName CustomEvent
  -> EventM ResourceName (Next (GameState n))
handleEvent s (AppEvent ()) = execStateT (tick >> checkDamage) s >>= continue
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey (KChar c) _)) =
  continue $ s &~ typeChar c &~ killEnemies
handleEvent s _ = continue s

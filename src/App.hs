{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import Actions.Enemy
import Actions.Health
import Actions.Words
import Brick
import Control.Lens
import Control.Monad.State
import Data.GameState
import Data.Void
import Display.Attrs
import Display.Render
import Graphics.Vty.Input.Events

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
handleEvent s (AppEvent ()) =
  let loop = do
        incTimeSinceHit
        stepEnemies
        spawnEnemies
        checkDamage
  in  execStateT loop s >>= continue
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent (EvKey (KChar c) _)) =
  continue $ s &~ typeChar c &~ killEnemies
handleEvent s _ = continue s

{-# LANGUAGE DataKinds #-}
module App where

import Actions.Enemy
import Actions.Health
import Actions.Words
import Brick
import Control.Lens
import Control.Monad.State
import Types
import Display.Attrs
import Display.Render
import Graphics.Vty.Input.Events

type ResourceName = String
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
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s e = if s ^. hp <= 0
  then case e of
    VtyEvent (EvKey (KChar ' ') []) -> continue $ resetGameState s
    _                               -> continue s
  else case e of
    AppEvent () ->
      let loop = do
            incTimeSinceHit
            stepEnemies
            spawnEnemies
            checkDamage
      in  execStateT loop s >>= continue
    VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
    VtyEvent (EvKey (KChar c  ) _      ) -> continue $ s &~ typeChar c &~ killEnemies
    _ -> continue s

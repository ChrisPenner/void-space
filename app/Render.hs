{-# LANGUAGE OverloadedStrings #-}

module Render where

import           Control.Lens
import           Data.Monoid
import           Brick
import           Brick.Widgets.Core
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Words
import           Dashboard
import qualified Data.Text                     as T
import           Stars
import           GameState
import           Enemies

drawCorridor :: GameState -> Widget n
drawCorridor s = txt (s ^. ship . coerced) <+> drawEnemies s

drawGame :: GameState -> [Widget n]
drawGame s =
  [ header
       -- , centerLayer (shipWithWords (s ^. wordState) (s ^. ship))
  , vCenterLayer $ drawCorridor s
  , stars <=> dashboard
  , stars
  ]

drawEnemies :: GameState -> Widget n
drawEnemies s =
  let rows = s ^.. enemiesState . enemies . traversed . withIndex . to enemyRow
      enemyRow (i, e) =
        T.pack (take (e ^. distance) (infiniteStarField i)) <> e ^. word
  in  txt . T.unlines $ rows

shipWithWords :: WordState -> Ship -> Widget n
shipWithWords ws (Ship shipText) =
  padAll 4 (mkWordWidget L ws)
    <+> (   (padBottom (Pad 1) . padLeft (Pad 5) $ mkWordWidget U ws)
        <=> txt shipText
        <=> (padTop (Pad 1) . padLeft (Pad 5) $ mkWordWidget D ws)
        )
    <+> padAll 4 (mkWordWidget R ws)

header :: Widget n
header = hCenterLayer (txt "VOIDSPACE")


-- shipText :: T.Text
-- shipText
--   = "\
-- \        //-A-\\\\\n\
-- \  ___---=======---___\n\
-- \(=__\\   /.. ..\\   /__=)\n\
-- \     ---\\__O__/---"


--        //-A-\\
--  ___---=======---___
--(=__\   /.. ..\   /__=)
--     ---\__O__/---

-- ship :: Widget n
-- ship = txt "\
-- \   /\\\n\
-- \  8  8\n\
-- \  8  8\n\
-- \ /|/\\|\\\n\
-- \/_||||_\\\n"

{-# LANGUAGE OverloadedStrings #-}

module Render where

import Control.Lens
import Data.Monoid
import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import Words
import Dashboard
import qualified Data.Text as T
import Stars
import GameState


drawGame :: GameState -> [Widget n]
drawGame s =
  [ header
  , wordWidget (s ^. wordState) <+> centerLayer ship
  , stars <=> dashboard
  , stars
  ]

header :: Widget n
header = hCenterLayer (txt "VOIDSPACE")


ship :: Widget n
ship = txt shipText

shipText :: T.Text
shipText
  = "\
\        //-A-\\\\\n\
\  ___---=======---___\n\
\(=__\\   /.. ..\\   /__=)\n\
\     ---\\__O__/---"


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

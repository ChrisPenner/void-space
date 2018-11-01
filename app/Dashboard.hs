{-# LANGUAGE OverloadedStrings #-}
module Dashboard where

import Brick
import Brick.Widgets.ProgressBar
import Brick.Widgets.Border.Style
import Brick.Widgets.Border

dashboard :: Widget n
dashboard = border (healthBar <=> shieldsBar)

shieldsBar =
  withBorderStyle unicodeBold
    . overrideAttr progressCompleteAttr "shields"
    $ progressBar  (Just "shields")     0.3
healthBar =
  withBorderStyle unicodeBold
    . overrideAttr progressCompleteAttr "health"
    $ progressBar  (Just "health")      0.9

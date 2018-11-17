{-# LANGUAGE OverloadedStrings #-}
module Dashboard where

import Brick
import Brick.Widgets.ProgressBar
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Health
import Control.Lens

import Attrs

dashboard :: (HasHealth s) => s -> Widget n
dashboard s = border (healthBar s <=> shieldsBar s)

shieldsBar :: (HasHealth s) => s -> Widget n
shieldsBar s =
  withBorderStyle unicodeBold
    . overrideAttr progressCompleteAttr shieldsAttr
    $ progressBar  (Just "shields")     (s ^. health . shields)

healthBar :: (HasHealth s) => s -> Widget n
healthBar s =
  withBorderStyle unicodeBold
    . overrideAttr progressCompleteAttr healthAttr
    $ progressBar  (Just "health")      (s ^. health . hp)

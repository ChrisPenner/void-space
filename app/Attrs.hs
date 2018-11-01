{-# LANGUAGE OverloadedStrings #-}
module Attrs where

import Brick
import Graphics.Vty.Attributes
import Brick.Widgets.ProgressBar

data AttrTypes = Health | Shields


attrs :: AttrMap
attrs =
  let def      = defAttr
      mappings = [("health", white `on` red), ("shields", white `on` blue)]
  in  attrMap defAttr mappings

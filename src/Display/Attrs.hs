{-# LANGUAGE OverloadedStrings #-}
module Display.Attrs where

import Brick
import Graphics.Vty.Attributes
import Brick.Widgets.ProgressBar

healthAttr :: AttrName
healthAttr = "health"

shieldsAttr :: AttrName
shieldsAttr = "shields"

typedAttr :: AttrName
typedAttr = "typed"

untypedAttr :: AttrName
untypedAttr = "untyped"

attrs :: AttrMap
attrs =
  let def = defAttr
      mappings =
        [ (healthAttr , white `on` red)
        , (shieldsAttr, white `on` blue)
        , (typedAttr  , magenta `on` black)
        , (untypedAttr, cyan `on` black)
        ]
  in  attrMap defAttr mappings

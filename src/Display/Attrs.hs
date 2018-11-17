{-# LANGUAGE OverloadedStrings #-}
module Display.Attrs where

import Brick
import Graphics.Vty.Attributes

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
  let mappings =
        [ (healthAttr , white `on` red)
        , (shieldsAttr, black `on` cyan)
        , (typedAttr  , magenta `on` black)
        , (untypedAttr, cyan `on` black)
        ]
  in  attrMap defAttr mappings

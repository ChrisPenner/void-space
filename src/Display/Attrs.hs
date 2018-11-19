{-# LANGUAGE OverloadedStrings #-}
module Display.Attrs where

import           Brick
import           Graphics.Vty.Attributes

healthAttr :: AttrName
healthAttr = "health"

shieldsAttr :: AttrName
shieldsAttr = "shields"

typedAttr :: AttrName
typedAttr = "typed"

untypedAttr :: AttrName
untypedAttr = "untyped"

wormholeAttr :: AttrName
wormholeAttr = "wormhole"

shipAttr :: AttrName
shipAttr = "ship"

redAttr :: AttrName
redAttr = "red"

cyanAttr :: AttrName
cyanAttr = "cyan"

attrs :: AttrMap
attrs =
  let mappings =
        [ (healthAttr  , white `on` red)
        , (shieldsAttr , black `on` cyan)
        , (typedAttr   , defAttr `withForeColor` magenta)
        , (untypedAttr , defAttr `withForeColor` cyan)
        , (wormholeAttr, defAttr `withForeColor` yellow)
        , (shipAttr    , defAttr `withForeColor` green)
        , (redAttr     , defAttr `withForeColor` red)
        , (cyanAttr    , defAttr `withForeColor` cyan)
        ]
  in  attrMap defAttr mappings

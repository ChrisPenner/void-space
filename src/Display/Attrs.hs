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
        , (typedAttr   , magenta `on` black)
        , (untypedAttr , cyan `on` black)
        , (wormholeAttr, yellow `on` black)
        , (shipAttr    , green `on` black)
        , (redAttr     , red `on` black)
        , (cyanAttr    , cyan `on` black)
        ]
  in  attrMap defAttr mappings

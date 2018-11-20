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

criticalShieldAttr :: AttrName
criticalShieldAttr = "criticalShield"

damagedShieldAttr :: AttrName
damagedShieldAttr = "damagedShield"

hurtShieldAttr :: AttrName
hurtShieldAttr = "hurtShield"

mediumShieldAttr :: AttrName
mediumShieldAttr = "mediumShield"

goodShieldAttr :: AttrName
goodShieldAttr = "goodShield"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

attrs :: AttrMap
attrs =
  let mappings =
        [ (healthAttr        , white `on` red)
        , (shieldsAttr       , black `on` cyan)
        , (typedAttr         , defAttr `withForeColor` magenta)
        , (untypedAttr       , defAttr `withForeColor` cyan)
        , (wormholeAttr      , defAttr `withForeColor` yellow)
        , (shipAttr          , defAttr `withForeColor` green)
        , (gameOverAttr      , defAttr `withForeColor` red)
        , (criticalShieldAttr, defAttr `withForeColor` red `withStyle` bold)
        , (damagedShieldAttr , defAttr `withForeColor` magenta `withStyle` bold)
        , (hurtShieldAttr    , defAttr `withForeColor` yellow `withStyle` bold)
        , (mediumShieldAttr  , defAttr `withForeColor` blue `withStyle` bold)
        , (goodShieldAttr    , defAttr `withForeColor` cyan `withStyle` bold)
        ]
  in  attrMap defAttr mappings

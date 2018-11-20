{-# LANGUAGE MultiWayIf #-}
module Display.Shield where

import Brick
import Display.Attrs

shieldWidget :: Int -> Float -> Widget n
shieldWidget height health =
  let halfShield = toLine <$> reverse [1 .. (height `div` 2) + 1]
      colour     = if
        | health < 0.2 -> redAttr
        | health < 0.4 -> magentaAttr
        | health < 0.6 -> yellowAttr
        | health < 0.8 -> blueAttr
        | otherwise    -> cyanAttr
  in  withAttr colour . str . unlines $ reverse halfShield <> halfShield

-- Generate a line of a parabolic shield
toLine :: Int -> String
toLine n = replicate ((n * n) `div` 4) ' ' <> "#"

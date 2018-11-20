{-# LANGUAGE MultiWayIf #-}
module Display.Shield where

import Brick
import Display.Attrs

shieldWidget :: Int -> Float -> Widget n
shieldWidget height health =
  let halfHeight = (ceiling (fromIntegral height / 2 :: Float))
      maxWidth   = parabolic halfHeight
      halfShield = toLine maxWidth . parabolic <$> reverse [1 .. halfHeight]
      colour     = if
        | health < 0.2 -> criticalShieldAttr
        | health < 0.4 -> damagedShieldAttr
        | health < 0.6 -> hurtShieldAttr
        | health < 0.8 -> mediumShieldAttr
        | otherwise    -> goodShieldAttr
  in  withAttr colour
      .  str
      .  unlines
      .  fmap reverse
      $  halfShield
      <> reverse halfShield

parabolic :: Int -> Int
parabolic n = (n * n) `div` 4

-- Generate a line of a parabolic shield
toLine :: Int -> Int -> String
toLine maxWidth n = replicate n ' ' <> "#" <> replicate (maxWidth - n) ' '

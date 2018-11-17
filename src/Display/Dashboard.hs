{-# LANGUAGE OverloadedStrings #-}
module Display.Dashboard where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.ProgressBar
import Control.Lens
import Data.Health
import Display.Attrs
import qualified Data.Text as T
import Data.GameState
import Text.Printf

dashboard :: (HasHealth s, HasGameState s n) => s -> Widget r
dashboard s = healthArea s <+> killCounter s

healthArea :: (HasHealth s) => s -> Widget r
healthArea s = border (healthBar s <=> shieldsBar s)

killCounter :: (HasGameState s n) => s -> Widget r
killCounter s =
  let scoreText = T.pack . show $ (s ^. score)
  in  borderWithLabel (txt "Kills")
      . padBottom (Pad 1)
      . padLeftRight 3
      . txt
      $ scoreText

displayHealthAmount :: String -> Float -> String
displayHealthAmount word amt = printf "%s %.0f%%" word (amt * 100)

shieldsBar :: (HasHealth s) => s -> Widget r
shieldsBar s =
  withBorderStyle unicodeBold
    . overrideAttr progressCompleteAttr shieldsAttr
    $ progressBar (Just $ displayHealthAmount "shields" (s ^. shields))
                  (s ^. shields)

healthBar :: (HasHealth s) => s -> Widget r
healthBar s =
  withBorderStyle unicodeBold
    . overrideAttr progressCompleteAttr                            healthAttr
    $ progressBar  (Just $ displayHealthAmount "health" (s ^. hp)) (s ^. hp)

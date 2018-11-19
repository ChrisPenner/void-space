{-# LANGUAGE OverloadedStrings #-}
module Display.Dashboard where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.ProgressBar
import Control.Lens
import Types
import Display.Attrs
import Text.Printf
import Text.Wrap
import qualified Data.Text as T

dashboard :: (HasHealth s, HasGameState s) => s -> Widget r
dashboard s = killCounter s <=> healthArea s

healthArea :: (HasHealth s) => s -> Widget r
healthArea s = border (healthBar s <=> shieldsBar s)

killCounter :: (HasGameState s) => s -> Widget r
killCounter s =
  let scoreText = if (s ^. score) <= 0
        then T.singleton '-'
        else T.replicate (s ^. score) $ T.singleton 'x'
  in  borderWithLabel (txt "Kills")
      . padRight Max
      . padLeft (Pad 1)
      . txtWrapWith
          (WrapSettings {preserveIndentation = False, breakLongWords = True})
      $ scoreText

displayHealthAmount :: String -> Float -> String
displayHealthAmount w amt = printf "%s %.0f%%" w (amt * 100)

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

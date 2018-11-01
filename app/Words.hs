{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Words where

import Brick
import Brick.Markup
import qualified Data.Text as T

import Attrs

data WordState = WordState (T.Text, T.Text)

wordWidget :: WordState -> Widget n
wordWidget (WordState (typed, untyped)) =
  markup $ (typed @? typedAttr <> untyped @? untypedAttr)

typeKey :: Char -> WordState -> WordState
typeKey c w@(WordState (untyped, T.uncons -> Just (h, rest))) | h == c =
  WordState (untyped <> T.singleton h, rest)
typeKey _ w = w

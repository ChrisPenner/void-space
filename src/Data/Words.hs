{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Words where

import           Brick
import           Brick.Markup
import qualified Data.Text                     as T
import           Control.Lens                  as L
import qualified Data.Stream.Infinite          as S
import           Display.Attrs

data WordT = WordT { _typed :: T.Text, _untyped :: T.Text }
makeLenses ''WordT

class HasWords s where
  eachWord :: IndexedTraversal' Int s WordT

class HasWordStream s where
  wordStream :: Lens' s (S.Stream T.Text)

wordWidget :: WordT -> Widget n
wordWidget w =
  markup ((w ^. typed) @? typedAttr <> (w ^. untyped) @? untypedAttr)

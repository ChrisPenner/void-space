module Display.Words where

import Brick
import Brick.Markup
import Control.Lens
import Display.Attrs
import Types


wordWidget :: WordT -> Widget n
wordWidget w =
  markup ((w ^. typed) @? typedAttr <> (w ^. untyped) @? untypedAttr)

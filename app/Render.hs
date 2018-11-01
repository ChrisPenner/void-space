{-# LANGUAGE OverloadedStrings #-}

module Render where

import Data.Monoid
import Brick
import qualified Data.Text as T

-- ship :: Widget n
-- ship = txt "\
-- \   /\\\n\
-- \  (  )\n\
-- \  (  )\n\
-- \ /|/\\|\\\n\
-- \/_||||_\\\n"


ship :: Widget n
ship = txt shipText

shipText :: T.Text
shipText
  = "\
\        //-A-\\\\\n\
\  ___---=======---___\n\
\(=__\\   /.. ..\\   /__=)\n\
\     ---\\__O__/---"


--        //-A-\\
--  ___---=======---___
--(=__\   /.. ..\   /__=)
--     ---\__O__/---

{-# LANGUAGE OverloadedStrings #-}

module Render where

import Brick
import qualified Data.Text as T

ship :: Widget n
ship = txt "\
\   /\\\n\
\  (  )\n\
\  (  )\n\
\ /|/\\|\\\n\
\/_||||_\\\n"

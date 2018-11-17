{-# LANGUAGE TemplateHaskell #-}
module Health where

import Control.Lens

data Health = Health
  { _hp :: Float, _shields :: Float }

makeClassy ''Health

startHealth :: Health
startHealth = Health 1 1

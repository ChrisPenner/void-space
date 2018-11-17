{-# LANGUAGE TemplateHaskell #-}
module Data.Health where

import Control.Lens

data Health = Health
  { _hp :: Float, _shields :: Float, _timeSinceHit :: Int}

makeClassy ''Health

startHealth :: Health
startHealth = Health 1 1 0

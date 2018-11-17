{-# LANGUAGE TemplateHaskell #-}

module Data.Ship where

import Control.Lens
import qualified Data.Text as T
import Control.Monad.State

newtype Ship = Ship { _ascii :: T.Text }

makeClassy ''Ship

corridorSize :: (HasShip s, MonadState s m) => m Int
corridorSize = uses (ship . ascii) (length . T.lines)

{-# LANGUAGE TemplateHaskell #-}

module Data.Art where

import Control.Lens
import qualified Data.Text as T
import Control.Monad.State

data Art = Art { _ship :: T.Text, _wormhole :: T.Text, _gameover :: T.Text }

makeClassy ''Art

corridorSize :: (HasArt s, MonadState s m) => m Int
corridorSize = uses ship (length . T.lines)

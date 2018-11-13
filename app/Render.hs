{-# LANGUAGE OverloadedStrings #-}

module Render where

import           Control.Lens
import           Data.Monoid
import           Brick
import           Brick.Widgets.Core
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Data.Text.Markup
import           Words
import           Dashboard
import qualified Data.Text                     as T
import           Stars
import           GameState
import           Enemies
import           Control.Lens.Selection
import           Data.Functor.Selection
import           Data.Functor.Compose
import           Ship
import           Data.Semigroup                as S
import qualified Data.Map                      as M
import           Control.Arrow                            ( (&&&) )
import           Data.List
import           Data.Foldable
import           Data.Maybe
import           Control.Monad.State

drawCorridor :: GameState n -> Widget r
drawCorridor s =
  txt (s ^. ship . coerced) <+> drawEnemies s (evalState corridorSize s)

drawGame :: GameState n -> [Widget r]
drawGame s =
  [header, vCenterLayer $ drawCorridor s, stars <=> dashboard, stars]

drawEnemies :: GameState n -> Int -> Widget r
drawEnemies s sz = vBox $ foldMap (pure . widgetForRow) [0 .. sz]
 where
  widgetForRow :: Int -> Widget n
  widgetForRow i =
    fromMaybe (txt "-") (sortedEnemies ^? ix i . to (toWidget i))
  toWidget i e =
    let widget = either txt focusedWordWidget (e ^. word)
    in  addPadding i (e ^. distance) widget
  addPadding i amt w = txt (T.pack $ take amt (infiniteStarField i)) <+> w
  sortedEnemies = M.fromList $ (_row &&& id) <$> sortOn
    _distance
    (s ^.. enemies . traversed . _Just)

header :: Widget n
header = hCenterLayer (txt "VOIDSPACE")

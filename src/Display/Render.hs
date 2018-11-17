{-# LANGUAGE OverloadedStrings #-}

module Display.Render where

import           Control.Lens
import           Brick
import           Brick.Widgets.Center
import           Data.Words
import           Display.Dashboard
import qualified Data.Text                     as T
import           Display.Stars
import           Data.GameState
import           Data.Enemies
import           Data.Art
import qualified Data.Map                      as M
import           Control.Arrow                            ( (&&&) )
import           Data.List
import           Data.Maybe
import           Control.Monad.State

drawWormhole :: GameState n -> Widget r
drawWormhole s = txt (s ^. wormhole)

drawCorridor :: GameState n -> Widget String
drawCorridor s =
  txt (s ^. ship)
    <+> hLimit 50 (padRight Max (drawEnemies s (evalState corridorSize s)))
    <+> drawWormhole s

drawGame :: GameState n -> [Widget String]
drawGame s =
  [header, vCenterLayer $ drawCorridor s, stars <=> dashboard s, stars]

drawEnemies :: GameState n -> Int -> Widget String
drawEnemies s sz = vBox $ foldMap (pure . widgetForRow) [0 .. sz]
 where
  widgetForRow :: Int -> Widget n
  widgetForRow i = fromMaybe (str . take 100 $ infiniteStarField i)
                             (sortedEnemies ^? ix i . to (toWidget i))
  toWidget i e =
    let widget = either txt focusedWordWidget (e ^. word)
    in  addPadding i (e ^. distance) widget
  addPadding i amt w = txt (T.pack $ take amt (infiniteStarField i)) <+> w
  sortedEnemies = M.fromList $ (_row &&& id) <$> sortOn
    _distance
    (s ^.. enemies . traversed . _Just)

header :: Widget n
header = hCenterLayer (txt "VOIDSPACE")

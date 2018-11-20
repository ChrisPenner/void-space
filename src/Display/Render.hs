{-# LANGUAGE OverloadedStrings #-}

module Display.Render where

import           Actions.Enemy
import           Brick
import           Brick.Markup
import           Brick.Widgets.Center
import           Config
import           Control.Arrow                            ( (&&&) )
import           Control.Lens
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Display.Attrs
import           Display.Dashboard
import           Display.Shield
import           Display.Stars
import           Display.Words
import           Types
import qualified Data.Map                      as M
import qualified Data.Text                     as T


drawWormhole :: GameState -> Widget r
drawWormhole s = withAttr wormholeAttr $ txt (s ^. wormhole)

drawCorridor :: GameState -> Widget String
drawCorridor s =
  let shield' = if (s ^. shields) > 0
        then shieldWidget (s ^. ship . to (length . T.lines)) (s ^. shields)
        else emptyWidget
  in  drawShip s
      <+> shield'
      <+> hLimit corridorWidth
                 (padRight Max (drawEnemies s (evalState corridorSize s)))
      <+> drawWormhole s

drawShip :: GameState -> Widget String
drawShip s = withAttr shipAttr $ txt (s ^. ship)

drawGame :: GameState -> [Widget String]
drawGame s = if s ^. hp <= 0
  then
    [ header
    , hCenterLayer . vCenterLayer $ gameOverWidget s
    , stars
    , stars <=> dashboard s
    ]
  else
    [ header
    , hCenterLayer . vCenterLayer $ drawCorridor s
    , stars <=> dashboard s
    ]

gameOverWidget :: GameState -> Widget String
gameOverWidget s = withAttr redAttr . txt $ s ^. gameover

drawEnemies :: GameState -> Int -> Widget String
drawEnemies s sz = vBox $ foldMap (pure . widgetForRow) [0 .. sz]
 where
  widgetForRow :: Int -> Widget n
  widgetForRow i = fromMaybe (str . take 100 $ infiniteStarField i)
                             (sortedEnemies ^? ix i . to (toWidget i))
  toWidget i e =
    let widget = wordWidget (e ^. word) in addPadding i (e ^. distance) widget
  addPadding i amt w = txt (T.pack $ take amt (infiniteStarField i)) <+> w
  sortedEnemies =
    M.fromList $ (_row &&& id) <$> sortOn _distance (s ^.. enemies . _Just)

header :: Widget n
header = hCenterLayer $ markup ("VOID" @? redAttr <> "SPACE" @? cyanAttr)

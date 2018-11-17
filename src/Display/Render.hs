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

drawCorridor :: GameState n -> Widget r
drawCorridor s = txt (s ^. ship) <+> drawEnemies s (evalState corridorSize s)

drawGame :: GameState n -> [Widget r]
drawGame s =
  [header, vCenterLayer $ drawCorridor s, stars <=> dashboard s, stars]

drawEnemies :: GameState n -> Int -> Widget r
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

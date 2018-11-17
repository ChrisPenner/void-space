module Display.Stars where

import           Brick.Types
import           Brick.Widgets.Core
import           Control.Lens
import qualified Data.Text                     as T
import           System.Random

stars :: Widget String
stars = cached "background-stars"
  $ Widget {hSize = Greedy, vSize = Greedy, render = renderStars}

renderStars :: RenderM n (Result n)
renderStars = do
  ctx <- getContext
  let width  = ctx ^. availWidthL
  let height = ctx ^. availHeightL
  render $ txt $ genStars width height

genStars :: Int -> Int -> T.Text
genStars width height =
  T.unlines
    . T.chunksOf width
    . T.pack
    . take (width * height)
    $ backgroundStars


-- | Memoize the the background one for performance reasons
backgroundStars :: String
backgroundStars = infiniteStarField 42

infiniteStarField :: Int -> String
infiniteStarField seed = toStar <$> probabilities
 where
  probabilities :: [Float]
  probabilities = randomRs (0, 1) (mkStdGen seed)
  toStar r | r < 0.002 = '*'
           | r < 0.025 = '.'
           | otherwise = ' '

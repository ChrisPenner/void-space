module Stars where

import           Brick.Types
import           Brick.BorderMap               as BM
import           Brick.Widgets.Core
import           Control.Lens
import qualified Data.Text                     as T
import           System.Random

stars :: Widget n
stars = Widget {hSize = Greedy, vSize = Greedy, render = renderStars}

renderStars :: RenderM n (Result n)
renderStars = do
  ctx <- getContext
  let width  = ctx ^. availWidthL
  let height = ctx ^. availHeightL
  render $ txt $ genStars width height

genStars :: Int -> Int -> T.Text
genStars width height = stars
 where
  probabilities :: [Float]
  probabilities = randomRs (0, 1) (mkStdGen 42)
  stars =
    T.unlines
      . T.chunksOf width
      . T.pack
      . fmap toStar
      . take (width * height)
      $ probabilities
  toStar r | r < 0.002 = '*'
           | r < 0.025 = '.'
           | otherwise = ' '

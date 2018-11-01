module Main where

import Lib
import Brick
import Control.Monad

import GameState
import App
import Data.List.NonEmpty
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = do
  (w : wordList) <- T.words <$> TIO.readFile "word-list.txt"
  void $ defaultMain app (gameStart (w :| wordList))

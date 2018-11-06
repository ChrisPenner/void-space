module Main where

import Lib
import Brick
import Control.Monad

import GameState
import App
import Data.List.NonEmpty
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Render

loadShip :: IO Ship
loadShip = Ship <$> TIO.readFile "./ships/colonial-viper.txt"

main :: IO ()
main = do
  ship           <- loadShip
  (w : wordList) <- T.words <$> TIO.readFile "word-list.txt"
  void $ defaultMain app (gameStart (w :| wordList) ship)

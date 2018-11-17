{-# LANGUAGE ViewPatterns #-}
module Main where

import Lib
import Brick
import Brick.BChan
import Control.Monad

import GameState
import App
import Data.List.NonEmpty as NE
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Render
import Data.Stream.Infinite as S
import Graphics.Vty
import Control.Concurrent
import Control.Concurrent.Async
import Ship

loadShip :: IO Ship
loadShip = Ship <$> TIO.readFile "./ships/colonial-viper.txt"

main :: IO ()
main = do
  let loadVty = standardIOConfig >>= mkVty
  bChan                     <- newBChan 10
  ship                      <- loadShip
  (NE.fromList -> wordList) <- T.words <$> TIO.readFile "word-list.txt"
  withAsync (timer bChan) . const . void $ customMain
    loadVty
    (Just bChan)
    app
    (gameStart (S.cycle wordList) ship)

millisecond :: Int
millisecond = 1000

timer :: BChan () -> IO ()
timer bChan = forever $ do
  writeBChan bChan ()
  threadDelay (300 * millisecond)

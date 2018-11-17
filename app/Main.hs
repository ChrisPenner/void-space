{-# LANGUAGE ViewPatterns #-}
module Main where

import Brick
import Brick.BChan
import Control.Monad

import Data.GameState
import App
import Data.List.NonEmpty as NE
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Stream.Infinite as S
import Graphics.Vty
import Control.Concurrent
import Control.Concurrent.Async
import Data.Art

loadArt :: IO Art
loadArt = Art <$> TIO.readFile "./art/colonial-viper.txt" <*> TIO.readFile
  "./art/wormhole.txt"

main :: IO ()
main = do
  let loadVty = standardIOConfig >>= mkVty
  bChan                     <- newBChan 10
  art'                      <- loadArt
  (NE.fromList -> wordList) <- T.words <$> TIO.readFile "word-list.txt"
  withAsync (timer bChan) . const . void $ customMain
    loadVty
    (Just bChan)
    app
    (gameStart (S.cycle wordList) art')

millisecond :: Int
millisecond = 1000

timer :: BChan () -> IO ()
timer bChan = forever $ do
  writeBChan bChan ()
  threadDelay (300 * millisecond)

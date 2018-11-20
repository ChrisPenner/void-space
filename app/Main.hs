{-# LANGUAGE ViewPatterns #-}
module Main where

import           Brick
import           Brick.BChan
import           Control.Monad

import           App
import           Config
import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.List.NonEmpty            as NE
import           Data.Stream.Infinite          as S
import           Graphics.Vty
import           Types
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

loadArt :: IO Art
loadArt =
  Art
    <$> TIO.readFile "./art/colonial-viper.txt"
    <*> TIO.readFile "./art/wormhole.txt"
    <*> TIO.readFile "./art/game-over.txt"

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
  threadDelay (tickTimeMilliseconds * millisecond)

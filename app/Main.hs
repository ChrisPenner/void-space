module Main where

import Lib
import Brick
import Control.Monad

import GameState
import App

main :: IO ()
main = void $ defaultMain app GameState

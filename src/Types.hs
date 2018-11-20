{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import qualified Data.Text                     as T
import           Control.Lens                  as L
import qualified Data.Stream.Infinite          as S
import           Control.Concurrent
import Control.Monad.State


data Art = Art { _ship :: T.Text, _wormhole :: T.Text, _gameover :: T.Text }
makeClassy ''Art

data WordT = WordT { _typed :: T.Text, _untyped :: T.Text }
makeLenses ''WordT

data Enemy = Enemy
  { _distance :: Int
  , _row :: Int
  , _word :: WordT
  }

makeLenses ''Enemy

type Enemies = [ Maybe Enemy ]

enemiesStart :: Int -> Enemies
enemiesStart n = replicate n Nothing

data Health = Health
  { _hp :: Float, _shields :: Float, _timeSinceHit :: Int}

makeClassy ''Health

startHealth :: Health
startHealth = Health 1 1 0


data GameState =
  GameState {
      _enemiesState :: Enemies
    , _artState :: Art
    , _wordStream' :: S.Stream T.Text
    , _healthState :: Health
    , _score :: Int
    , _speedVar :: MVar Int
    }

makeClassy ''GameState

gameStart :: S.Stream T.Text -> Art -> MVar Int -> GameState
gameStart aWordStream art' spdVar = GameState
  { _enemiesState = enemiesStart (art' ^. ship . to (length . T.lines))
  , _artState     = art'
  , _wordStream'  = aWordStream
  , _healthState  = startHealth
  , _score        = 0
  , _speedVar     = spdVar
  }

resetGameState :: MonadIO m => GameState -> m GameState
resetGameState g = do
  void . liftIO $ swapMVar (g ^. speedVar) 1
  return (gameStart (g ^. wordStream) (g ^. art) (g ^. speedVar))

class HasEnemies s where
  enemies :: IndexedTraversal' Int s (Maybe Enemy)

class HasWords s where
  eachWord :: IndexedTraversal' Int s WordT

class HasWordStream s where
  wordStream :: Lens' s (S.Stream T.Text)

instance HasWords Enemies where
  eachWord = traversed <. (_Just . word)

instance HasWords GameState where
  eachWord = enemiesState . eachWord

instance HasWordStream GameState  where
  wordStream = wordStream'

instance HasEnemies GameState  where
  enemies = enemiesState . traversed

instance HasArt GameState  where
  art = artState

instance HasHealth GameState where
  health = healthState

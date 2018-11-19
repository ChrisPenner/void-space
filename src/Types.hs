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
{-# LANGUAGE GADTs #-}

module Types where

import qualified Data.Text                     as T
import           Control.Lens                  as L
import qualified Data.Stream.Infinite          as S
import           Data.Vector.Sized             as V
import           GHC.TypeLits


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

newtype Enemies' n a = Enemies' (Vector n a)
  deriving (Functor, Foldable, Traversable)

type Enemies n = Enemies' n (Maybe Enemy)

enemiesStart :: (KnownNat n) => Enemies n
enemiesStart = Enemies' (V.generate (const Nothing))

data Health = Health
  { _hp :: Float, _shields :: Float, _timeSinceHit :: Int}

makeClassy ''Health

startHealth :: Health
startHealth = Health 1 1 0


data GameState n where
  GameState  :: KnownNat n => {
      _enemiesState :: Enemies n
    , _artState :: Art
    , _wordStream' :: S.Stream T.Text
    , _healthState :: Health
    , _score :: Int
    } -> GameState n

makeClassy ''GameState

gameStart :: (KnownNat n) => S.Stream T.Text -> Art -> GameState n
gameStart aWordStream art' = GameState
  { _enemiesState = enemiesStart
  , _artState     = art'
  , _wordStream'  = aWordStream
  , _healthState  = startHealth
  , _score        = 0
  }

resetGameState :: (KnownNat n) => GameState n -> GameState n
resetGameState g = gameStart (g ^. wordStream) (g ^. art)

class HasEnemies s where
  enemies :: IndexedTraversal' Int s (Maybe Enemy)

class HasWords s where
  eachWord :: IndexedTraversal' Int s WordT

class HasWordStream s where
  wordStream :: Lens' s (S.Stream T.Text)

instance HasWords (Enemies n) where
  eachWord = traversed <. (_Just . word)

instance HasWords (GameState n) where
  eachWord = enemiesState . eachWord

instance HasWordStream (GameState n) where
  wordStream = wordStream'

instance HasEnemies (GameState n) where
  enemies = enemiesState . traversed

instance HasArt (GameState n) where
  art = artState

instance HasHealth (GameState n) where
  health = healthState

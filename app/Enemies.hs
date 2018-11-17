{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Enemies where

import           Control.Lens                  as L
import           Brick
import           Brick.Markup
import           Control.Monad.State
import qualified Data.Text                     as T
import           Data.Functor.Compose
import           Data.Functor.Selection
import           Control.Lens.Selection
import           Data.Coerce
import           Control.Monad.Supply
import           Attrs
import           Words
import           System.Random
import           Ship
import           Data.Vector.Sized             as V
import           GHC.TypeLits
import           Data.Finite
import           Data.Functor.Rep
import           Data.Distributive
import           Control.Lens.Indexed
import           Control.Applicative as A

data Enemy a = Enemy
  { _distance :: Int
  , _row :: Int
  , _word :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Enemy

instance HasWords (Enemy (Either T.Text FocusedWord)) where
  eachWord = traverse

type MEnemy = Maybe (Enemy (Either T.Text FocusedWord))

newtype Enemies n a = Enemies (Vector n a) deriving (Functor, Foldable, Traversable)
makeClassy ''Enemies

instance (KnownNat n) => Distributive (Enemies n) where
  distribute = distributeRep

instance (KnownNat n) => Representable (Enemies n) where
  type Rep (Enemies n) = Finite n
  index (Enemies v) = V.index v
  tabulate = Enemies . V.generate

instance (KnownNat n) => FunctorWithIndex (Finite n) (Enemies n) where
  imap = imapRep

instance (KnownNat n) => FoldableWithIndex (Finite n) (Enemies n) where
  ifoldMap = ifoldMapRep

instance (KnownNat n) =>TraversableWithIndex (Finite n) (Enemies n) where
  itraverse = itraverseRep

instance (HasWords a) => HasWords (Enemies n a) where
  eachWord = traversed . eachWord

enemiesStart :: (KnownNat n) => Enemies n (Maybe a)
enemiesStart = Enemies (V.generate (const Nothing))

stepEnemies :: (HasEnemies s n MEnemy, MonadState s m) => m ()
stepEnemies = enemies . traverse . _Just . distance -= 1

shouldSpawn :: (MonadIO m) => m Bool
shouldSpawn = (<= spawnPercentage) <$> liftIO (randomRIO (0, 1))
 where
  spawnPercentage :: Float
  spawnPercentage = 0.3

spawnEnemies
  :: ( MonadIO m
     , HasWordStream s
     , HasEnemies s n MEnemy
     , HasShip s
     , MonadState s m
     )
  => m ()
spawnEnemies = do
  shouldSpawn' <- shouldSpawn
  when shouldSpawn' newEnemy

newEnemy
  :: forall s n m
   . ( HasWordStream s
     , HasShip s
     , HasEnemies s n MEnemy
     , MonadState s m
     , MonadIO m
     )
  => m ()
newEnemy = do
  sz  <- corridorSize
  loc <- liftIO $ randomRIO (0, sz)
  w   <- getWord
  enemies . traversed . L.index loc %= addIfMissing loc w
 where
  addIfMissing loc w e@(Just _) = e
  addIfMissing loc w Nothing =
    Just $ Enemy {_row = loc, _distance = 50, _word = Left w}


killEnemies :: forall s n m . (HasEnemies s n MEnemy, MonadState s m) => m ()
killEnemies = (enemies . traversed) %= checkAlive
 where
  checkAlive :: MEnemy -> MEnemy
  checkAlive e =
    if has
         (_Just . word . choosing united
                                  (untyped . filtered (not . T.null) . united)
         )
         e
      then e
      else Nothing

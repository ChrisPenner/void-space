{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Enemies where

import Control.Lens
import Data.Words
import Data.Vector.Sized as V
import Data.Distributive
import Data.Functor.Rep
import GHC.TypeLits
import Data.Finite

data Enemy = Enemy
  { _distance :: Int
  , _row :: Int
  , _word :: WordT
  }

makeLenses ''Enemy

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

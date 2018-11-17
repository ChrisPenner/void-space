{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Words where

import           Brick
import           Brick.Markup
import qualified Data.Text                     as T
import           Control.Lens                  as L
import qualified Data.Stream.Infinite          as S
import           Display.Attrs

data FocusedWord = FocusedWord { _typed :: T.Text, _untyped :: T.Text }
makeLenses ''FocusedWord

type WordT = Either T.Text FocusedWord

class HasWords s where
  eachWord :: Traversal' s WordT

class HasWordStream s where
  wordStream :: Lens' s (S.Stream T.Text)

focusedWordWidget :: FocusedWord -> Widget n
focusedWordWidget fw =
  markup ((fw ^. typed) @? typedAttr <> (fw ^. untyped) @? untypedAttr)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Words where

import           Brick
import           Brick.Markup
import qualified Data.Text                     as T
import qualified Data.List.NonEmpty            as NE
import           Control.Lens                  as L
import qualified Data.Stream.Infinite          as S
import           Control.Comonad.Representable.Store
import           Data.Functor.Rep
import           Data.Distributive
import           Control.Lens.Selection
import           Data.Functor.Selection
import           Control.Monad.State
import           Data.Monoid
import           Control.Comonad
import           Attrs

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


typeChar :: (HasWords s, HasWordStream s, MonadState s m) => Char -> m ()
typeChar c = do
  result <- gets (failover (eachWord . _Right) (tryType c))
  case result of
    Just newEnemies -> put newEnemies
    Nothing         -> startWord c

startWord :: (HasWords s, MonadState s m) => Char -> m ()
startWord (T.singleton -> c) = do
  let doesMatch (Left  txt) = c `T.isPrefixOf` txt
      doesMatch (Right _  ) = False
  match <- gets $ failover (eachWord . filtered doesMatch) startTyping
  case match of
    Just w  -> put w
    Nothing -> pure ()

startTyping :: Either T.Text FocusedWord -> Either T.Text FocusedWord
startTyping x@(Left (T.uncons -> Just (c, rest))) =
  Right (FocusedWord (T.singleton c) rest)
startTyping x = x

getWord :: (HasWordStream s, MonadState s m) => m T.Text
getWord = do
  (a S.:> as) <- use wordStream
  wordStream .= as
  return a

tryType :: Char -> FocusedWord -> FocusedWord
tryType c w@(view untyped -> T.uncons -> Just (h, rest)) | h == c =
  ((w & typed %~ (|> h) & untyped %~ T.tail))
tryType _ w = w

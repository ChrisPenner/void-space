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
import GameState
import Enemies


typeChar :: MonadState GameState m => Char -> m ()
typeChar c = do
  result <- gets (failover (enemies . traversed . word . _Right) (tryType c))
  case result of
    Just newEnemies -> put newEnemies >> refreshWords
    Nothing         -> startWord c

startWord :: (HasEnemyState s, MonadState s m) => Char -> m ()
startWord (T.singleton -> c) = do
  let doesMatch (Left  txt) = c `T.isPrefixOf` txt
      doesMatch (Right _  ) = False
  match <- gets
    $ failover (enemies . traversed . word . filtered doesMatch) startTyping
  case match of
    Just w  -> put w
    Nothing -> pure ()

startTyping :: Either T.Text FocusedWord -> Either T.Text FocusedWord
startTyping x@(Left (T.uncons -> Just (c, rest))) =
  Right (FocusedWord (T.singleton c) rest)
startTyping x = x

getWord :: MonadState GameState m => m T.Text
getWord = do
  (a S.:> as) <- use wordStream
  wordStream .= as
  return a

tryType :: Char -> FocusedWord -> FocusedWord
tryType c w@(view untyped -> T.uncons -> Just (h, rest)) | h == c =
  ((w & typed %~ (|> h) & untyped %~ T.tail))
tryType _ w = w

refreshWords :: forall m . MonadState GameState m => m ()
refreshWords = get >>= (enemies . traversed . word %%~ setNewWord) >>= put
 where
  setNewWord :: Either T.Text FocusedWord -> m (Either T.Text FocusedWord)
  setNewWord (Right (FocusedWord _ "")) = Left <$> getWord
  setNewWord x                          = pure x

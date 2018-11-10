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

data WordState = WordState
  { _focused :: Maybe FocusedWord
  , _wordList :: S.Stream T.Text
  }
makeLenses ''WordState

wordStart :: S.Stream T.Text -> WordState
wordStart (upWord S.:> downWord S.:> leftWord S.:> rightWord S.:> wordSupply) =
  WordState {_focused = Nothing, _wordList = wordSupply}

-- wordWidget :: WordState -> Widget n
-- wordWidget (view focused -> Just fw) =
--   markup ((fw ^. typed) @? typedAttr <> (fw ^. untyped) @? untypedAttr)
-- wordWidget w = txt . T.unlines $ (w ^.. sections . folded)


focusedWordWidget :: FocusedWord -> Widget n
focusedWordWidget fw =
  markup ((fw ^. typed) @? typedAttr <> (fw ^. untyped) @? untypedAttr)

tryType :: Char -> FocusedWord -> FocusedWord
tryType c w@(view untyped -> T.uncons -> Just (h, rest)) | h == c =
  (w & typed %~ (|> h) & untyped %~ T.tail)
tryType _ w = w

-- typeKey :: Char -> State WordState ()
-- typeKey c = do
--   mFocused <- use focused
--   case mFocused of
--     Just fw -> do
--       focused . _Just .= tryType c fw
--       focused %= \case
--         Just (view untyped -> "") -> Nothing
--         fw                        -> fw
--     Nothing -> startWord c

-- startWord :: Char -> State WordState ()
-- startWord c = do
--   ws <- use sections
--   case ifind checkMatch ws of
--     Nothing          -> return ()
--     Just (loc, word) -> do
--       (newWord S.:> _) <- wordList <<%= S.tail
--       sections . itraversed . L.index loc .= newWord
--       focused ?= FocusedWord {_untyped = T.tail word, _typed = T.singleton c}
--   where checkMatch _ t = T.isPrefixOf (T.singleton c) t

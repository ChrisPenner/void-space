{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Words where

import           Brick
import           Brick.Markup
import qualified Data.Text                     as T
import qualified Data.List.NonEmpty            as NE
import           Control.Lens
import qualified Data.Stream.Infinite          as S

import           Attrs

data WordState = WordState
  { _currentWord :: (T.Text, T.Text), _wordList :: S.Stream T.Text
                           }
makeLenses ''WordState

wordWidget :: WordState -> Widget n
wordWidget WordState { _currentWord = (typed, untyped) } =
  markup (typed @? typedAttr <> untyped @? untypedAttr)

typeKey :: Char -> WordState -> WordState
typeKey c w@(WordState { _currentWord = (untyped, T.uncons -> Just (h, rest)) })
  | h == c = w & currentWord .~ (untyped <> T.singleton h, rest) & checkFinished
 where
  checkFinished w = if T.null (w ^. currentWord . _2)
    then
      let next S.:> rest = w ^. wordList
      in  w & currentWord .~ ("", next) & wordList .~ rest
    else w
typeKey _ w = w

wordsStart :: NE.NonEmpty T.Text -> WordState
wordsStart (S.cycle -> firstWord S.:> rest) =
  WordState {_currentWord = ("", firstWord), _wordList = rest}

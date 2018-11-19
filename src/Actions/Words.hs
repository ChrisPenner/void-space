{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Actions.Words where

import Data.Words
import qualified Data.Text as T
import Data.Stream.Infinite as S
import Control.Monad.State
import Control.Lens
import Control.Applicative

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

anyStarted :: (HasWords s, MonadState s m) => m Bool
anyStarted = gets $ anyOf (eachWord . typed) (not . T.null)

typeChar :: (HasWords s, HasWordStream s, MonadState s m) => Char -> m ()
typeChar c = do
  anyStarted' <- anyStarted
  if anyStarted' then typeExisting c else startNew c

typeExisting :: (HasWords s, MonadState s m) => Char -> m ()
typeExisting c = do
  let isStarted w = w ^. typed /= ""
  let doesMatch (view untyped -> T.uncons -> Just (c', _)) = c == c'
      doesMatch _ = False
  mMatchedWordIndex <-
    preuse $ (eachWord . filtered (isStarted <&&> doesMatch) . asIndex)
  case mMatchedWordIndex of
    Nothing  -> return ()
    Just ind -> eachWord . index ind %= typeCharInWord c

startNew :: (HasWords s, MonadState s m) => Char -> m ()
startNew c = do
  let doesMatch w = T.singleton c `T.isPrefixOf` _untyped w
  firstMatchIndex <- gets
    $ findOf (eachWord . withIndex) ((doesMatch <&&> (T.null . _typed)) . snd)
  case firstMatchIndex of
    Nothing       -> pure ()
    Just (ind, _) -> eachWord . index ind %= typeCharInWord c

typeCharInWord :: Char -> WordT -> WordT
typeCharInWord c w@(_untyped -> T.uncons -> Just (c', _))
  | c == c'   = w & untyped %~ T.tail & typed %~ (|> c)
  | otherwise = w
typeCharInWord _ w = w

getWord :: (HasWordStream s, MonadState s m) => m T.Text
getWord = do
  (a S.:> as) <- use wordStream
  wordStream .= as
  return a

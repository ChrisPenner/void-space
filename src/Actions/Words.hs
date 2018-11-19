{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Actions.Words where

import Data.Words
import qualified Data.Text as T
import Data.Stream.Infinite as S
import Control.Monad.State
import Control.Lens

typeChar :: (HasWords s, HasWordStream s, MonadState s m) => Char -> m ()
typeChar c = do
  result <- gets (failover eachWord (tryType c))
  case result of
    Just newEnemies -> put newEnemies
    Nothing         -> startWord c

startWord :: (HasWords s, MonadState s m) => Char -> m ()
startWord c = do
  let doesMatch w | _typed w == T.empty =
        T.singleton c `T.isPrefixOf` _untyped w
  firstMatchIndex <- gets $ findOf (eachWord . withIndex) (doesMatch . snd)
  case firstMatchIndex of
    Nothing       -> pure ()
    Just (ind, _) -> eachWord . index ind %= startTyping c

startTyping :: Char -> WordT -> WordT
startTyping c w@(_untyped -> T.uncons -> Just (c', rest))
  | c == c'   = w & untyped %~ T.tail & typed %~ (|> c)
  | otherwise = w

getWord :: (HasWordStream s, MonadState s m) => m T.Text
getWord = do
  (a S.:> as) <- use wordStream
  wordStream .= as
  return a

tryType :: Char -> WordT -> WordT
tryType c w@(view untyped -> T.uncons -> Just (h, _rest)) | h == c =
  ((w & typed %~ (|> h) & untyped %~ T.tail))
tryType _ w = w

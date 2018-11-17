{-# LANGUAGE ViewPatterns #-}
module Actions.Words where

import Data.Words
import qualified Data.Text as T
import Data.Stream.Infinite as S
import Control.Monad.State
import Control.Lens

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
startTyping (Left (T.uncons -> Just (c, rest))) =
  Right (FocusedWord (T.singleton c) rest)
startTyping x = x

getWord :: (HasWordStream s, MonadState s m) => m T.Text
getWord = do
  (a S.:> as) <- use wordStream
  wordStream .= as
  return a

tryType :: Char -> FocusedWord -> FocusedWord
tryType c w@(view untyped -> T.uncons -> Just (h, _rest)) | h == c =
  ((w & typed %~ (|> h) & untyped %~ T.tail))
tryType _ w = w

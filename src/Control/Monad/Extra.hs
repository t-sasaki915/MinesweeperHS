module Control.Monad.Extra (whenM, unlessM, orM) where

import           Control.Monad (unless, when)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mCond f = mCond >>= flip when f

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mCond f = mCond >>= flip unless f

orM :: Monad m => m Bool -> m Bool -> m Bool
orM mb1 mb2 = do
    b1 <- mb1
    b2 <- mb2
    pure (b1 || b2)

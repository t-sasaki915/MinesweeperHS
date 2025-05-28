module Control.Monad.Extra (whenM) where

import           Control.Monad (when)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mCond f = mCond >>= flip when f

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.RWS.CPS.Lens
    ( LazyRWST(..)
    ) where

import Control.Lens
import Control.Monad.Trans.RWS.CPS.Internal as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.RWS.CPS as Strict
import Control.Lens.Internal.Zoom
import Data.Profunctor.Unsafe

-- | This is a newtype wrapper only to provide a different Strict instance for
-- @Control.Monad.Trans.RWS.Lazy.RWST@ that converts to stricter
-- @Control.Monad.Trans.RWS.CPS.RWST@.
-- This is required becuase the Strict instances for @Control.Monad.Trans.RWS.Lazy.RWST@
-- is already defined to convert to @Control.Monad.Trans.RWS.Strict.RWST@.
-- This newtype doesn't deriving any instances as the intention is to immediately unwrap it
-- to use the underlying lazy RWST.
newtype LazyRWST r w s m a = LazyRWST { runLazyRWST :: Lazy.RWST r w s m a }

makeWrapped ''LazyRWST

toLazyRWST :: Monoid w => Strict.RWST r w s m a -> Lazy.RWST r w s m a
toLazyRWST (Strict.RWST m) = Lazy.RWST $ \r s -> m r s mempty
{-# INLINE toLazyRWST #-}

toStrictRWST :: (Functor m, Monoid w) => Lazy.RWST r w s m a -> Strict.RWST r w s m a
toStrictRWST (Lazy.RWST m) = Strict.RWST $ \r s w -> (\ ~(a, s', w') -> (a, s', w `mappend` w')) <$> m r s
{-# INLINE toStrictRWST #-}

-- | Based on code from Control.Lens.Iso
instance (Functor m, Monoid w) => Strict (LazyRWST r w s m a) (Strict.RWST r w s m a) where
  strict = iso (toStrictRWST . runLazyRWST) (LazyRWST . toLazyRWST)
  {-# INLINE strict #-}

-- | Unlike normal Wrapped instances, this doesn't simply peel off the newtype wrapper,
-- as that will expose the hidden CPS w state.
-- Based on code from Control.Lens.Wrapped
instance (Monoid w, Functor m, t ~ Strict.RWST r' w' s' m' a') => Rewrapped (Strict.RWST r w s m a) t
instance (Monoid w, Functor m) => Wrapped (Strict.RWST r w s m a) where
  type Unwrapped (Strict.RWST r w s m a) = r -> s -> m (a, s, w)
  _Wrapped' = iso Strict.runRWST Strict.rwsT
  {-# INLINE _Wrapped' #-}

-- | The Zoomed instance uses the RWST.Internal constructor to avoid a @Monoid w@ constraint.
-- | Based on code from Control.Lens.Zoomed
type instance Zoomed (Strict.RWST r w s z) = FocusingWith w z
instance Monad z => Zoom (Strict.RWST r w s z) (Strict.RWST r w t z) s t where
  zoom l (Strict.RWST m) = Strict.RWST $ \r s w ->
      (unfocusingWith #. l (FocusingWith #. (\s' -> m r s' w))) s
  {-# INLINE zoom #-}

-- | The Magnified instance uses the RWST.Internal constructor to avoid a @Monoid w@ constraint.
type instance Magnified (Strict.RWST a w s m) = EffectRWS w s m
instance Monad m => Magnify (Strict.RWST b w s m) (Strict.RWST a w s m) b a where
  magnify l (Strict.RWST m) = Strict.RWST $ \r s w ->
      (getEffectRWS #. l (EffectRWS #. (\r' s' -> m r' s' w))) r s
  {-# INLINE magnify #-}

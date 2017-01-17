{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.RWS.CPS.Lens where

import Control.Lens
import Control.Monad.Trans.RWS.CPS.Internal as Strict
import Control.Monad.RWS.CPS as Strict
import Control.Lens.Internal.Zoom
import Data.Profunctor.Unsafe

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

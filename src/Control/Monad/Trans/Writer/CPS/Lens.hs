{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Writer.CPS.Lens where

import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad.Trans.Writer.CPS.Internal as Strict
import Control.Monad.Writer.CPS as Strict
import Data.Profunctor.Unsafe

-- | Unlike normal Wrapped instances, this doesn't simply peel off the newtype wrapper,
-- as that will expose the hidden CPS w state.
-- Based on code from Control.Lens.Wrapped
instance (Monoid w, Functor m, t ~ Strict.WriterT w' m' a') => Rewrapped (Strict.WriterT w m a) t
instance (Monoid w, Functor m) => Wrapped (Strict.WriterT w m a) where
  type Unwrapped (Strict.WriterT w m a) = m (a, w)
  _Wrapped' = iso Strict.runWriterT Strict.writerT
  {-# INLINE _Wrapped' #-}

-- | The Zoomed instance uses the Strict.Internal constructor to avoid a @Monoid w@ constraint.
-- | Based on code from Control.Lens.Zoomed
type instance Zoomed (Strict.WriterT w m) = FocusingPlus w (Zoomed m)
instance Zoom m n s t => Zoom (Strict.WriterT w m) (Strict.WriterT w n) s t where
  zoom l (Strict.WriterT m) = Strict.WriterT $ \w ->
      zoom (\afb -> unfocusingPlus #. l (FocusingPlus #. afb)) (m w)
  {-# INLINE zoom #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Writer.CPS.Lens
    ( LazyWriterT(..)
    ) where

import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad.Trans.Writer.CPS as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Writer.CPS as Strict
import Data.Profunctor.Unsafe

-- | This is a newtype wrapper only to provide a different Strict instance for
-- @Control.Monad.Trans.Writer.Lazy.WriterT@ that converts to stricter
-- @Control.Monad.Trans.Writer.CPS.WriterT@.
-- This is required becuase the Strict instances for @Control.Monad.Trans.Writer.Lazy.WriterT@
-- is already defined to convert to @Control.Monad.Trans.Writer.Strict.WriterT@.
-- This newtype doesn't deriving any instances as the intention is to immediately unwrap it
-- to use the underlying lazy WriterT.
newtype LazyWriterT w m a = LazyWriterT { runLazyWriterT :: Lazy.WriterT w m a }
    deriving Show

makeWrapped ''LazyWriterT

toLazyWriterT :: Monoid w => Strict.WriterT w m a -> Lazy.WriterT w m a
toLazyWriterT (Strict.WriterT m) = Lazy.WriterT $ m mempty
{-# INLINE toLazyWriterT #-}

toStrictWriterT :: (Functor m, Monoid w) => Lazy.WriterT w m a -> Strict.WriterT w m a
toStrictWriterT (Lazy.WriterT m) = Strict.WriterT $ \w -> (\ ~(a, w') -> (a, w `mappend` w')) <$> m
{-# INLINE toStrictWriterT #-}

-- | Based on code from Control.Lens.Iso
instance (Functor m, Monoid w) => Strict (LazyWriterT w m a) (Strict.WriterT w m a) where
  strict = iso (toStrictWriterT . runLazyWriterT) (LazyWriterT . toLazyWriterT)
  {-# INLINE strict #-}

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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Monad
  ( module Smos.Monad,
    module Control.Monad.Reader,
    module Control.Monad.State,
  )
where

import Brick.Types (EventM)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.Resource as Resource (InternalState)
import Control.Monad.Trans.Resource.Internal (unResourceT)
import Control.Monad.Writer
import Import

newtype MkSmosM n c s a = MkSmosM
  { unMkSmosM :: WriterT [Text] (ReaderT c (ResourceT (EventM n s))) a
  }
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState s,
      MonadReader c,
      MonadWriter [Text],
      MonadResource
    )

runMkSmosM :: Resource.InternalState -> c -> MkSmosM n c s a -> EventM n s (a, [Text])
runMkSmosM res conf (MkSmosM act) =
  unResourceT (runReaderT (runWriterT act) conf) res

liftEventM :: EventM n s a -> MkSmosM n c s a
liftEventM func = MkSmosM $ lift $ lift $ lift func

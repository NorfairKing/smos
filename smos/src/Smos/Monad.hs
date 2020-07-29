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

import Brick.Types as B hiding (Next)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Import

newtype MkSmosM c n s a
  = MkSmosM
      { unMkSmosM :: NextT (StateT s (WriterT [Text] (ReaderT c (EventM n)))) a
      }
  deriving (Generic, Functor, Applicative, Monad, MonadState s, MonadReader c, MonadWriter [Text])

instance MonadIO (MkSmosM c n s) where
  liftIO = MkSmosM . liftIO

runMkSmosM :: c -> s -> MkSmosM c n s a -> EventM n ((MStop a, s), [Text])
runMkSmosM conf initState act = runReaderT (runWriterT (runStateT (runNextT (unMkSmosM act)) initState)) conf

data MStop a
  = Stop
  | Continue a
  deriving (Show, Eq, Generic)

instance Validity a => Validity (MStop a)

instance Functor MStop where
  fmap _ Stop = Stop
  fmap f (Continue a) = Continue $ f a

newtype NextT m a
  = NextT
      { runNextT :: m (MStop a)
      }

mapNextT :: (m (MStop a) -> n (MStop b)) -> NextT m a -> NextT n b
mapNextT f = NextT . f . runNextT

instance Functor m => Functor (NextT m) where
  fmap f (NextT func) = NextT $ fmap (f <$>) func

instance Monad m => Applicative (NextT m) where
  pure = NextT . pure . Continue
  (NextT f1) <*> (NextT f2) =
    NextT $ do
      n1 <- f1
      case n1 of
        Stop -> pure Stop
        Continue f -> do
          n2 <- f2
          pure $ f <$> n2

instance Monad m => Monad (NextT m) where
  (NextT ma) >>= fm =
    NextT $ do
      na <- ma
      case na of
        Stop -> pure Stop
        Continue a -> runNextT $ fm a

instance MonadTrans NextT where
  lift func = NextT $ Continue <$> func

instance MonadIO m => MonadIO (NextT m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (NextT m) where
  get = NextT $ gets Continue
  put = NextT . fmap Continue . put

instance MonadReader s m => MonadReader s (NextT m) where
  ask = NextT $ asks Continue
  local func (NextT m) = NextT $ local func m

instance MonadWriter w m => MonadWriter w (NextT m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapNextT $ \m -> do
    (a, w) <- listen m
    return $ fmap (\r -> (r, w)) a
  pass = mapNextT $ \m -> pass $ do
    a <- m
    return $ case a of
      Stop -> (Stop, id)
      Continue (v, f) -> (Continue v, f)

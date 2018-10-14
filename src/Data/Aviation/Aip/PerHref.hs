{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.PerHref(
  PerHref(..)
, ioPerHref
, nothingPerHref
, hrefPerHref
, basedirPerHref
, downloaddirPerHref
, logPerHref
, logeachPerHref
, logShowPerHref
, PerHrefAipCon
, PerHrefIO
) where

import Control.Category((.))
import Control.Applicative(Applicative(pure, (<*>)), Alternative((<|>), empty))
import Control.Lens hiding ((<.>))
import Control.Monad(Monad(return, (>>=)))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Aviation.Aip.AipCon(AipCon)
import Data.Aviation.Aip.Href(Href)
import Data.Functor(Functor(fmap))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.String(String)
import Prelude(Show(show))
import System.FilePath(FilePath)
import System.IO(IO)

newtype PerHref f a =
  PerHref
    (Href -> FilePath -> FilePath -> (String -> AipCon ()) -> f a)

instance Functor f => Functor (PerHref f) where
  fmap f (PerHref x) =
    PerHref (\h d d' l -> fmap f (x h d d' l))

instance Apply f => Apply (PerHref f) where
  PerHref f <.> PerHref a =
    PerHref (\h d d' l -> f h d d' l <.> a h d d' l)

instance Applicative f => Applicative (PerHref f) where
  pure =
    PerHref . pure . pure . pure . pure . pure

  PerHref f <*> PerHref a =
    PerHref (\h d d' l -> f h d d' l <*> a h d d' l)

instance Bind f => Bind (PerHref f) where
  PerHref x >>- f =
    PerHref (\h d d' l -> x h d d' l >>- \a -> let g = f a ^. _Wrapped in g h d d' l)

instance Monad f => Monad (PerHref f) where
  return =
    pure
  PerHref x >>= f =
    PerHref (\h d d' l -> x h d d' l >>= \a -> let g = f a ^. _Wrapped in g h d d' l)

instance Alt f => Alt (PerHref f) where
  PerHref x <!> PerHref y =
    PerHref (\h d d' l -> x h d d' l <!> y h d d' l)

instance Alternative f => Alternative (PerHref f) where
  PerHref x <|> PerHref y =
    PerHref (\h d d' l -> x h d d' l <|> y h d d' l)
  empty =
    (PerHref . pure . pure . pure . pure) empty

instance MonadTrans PerHref where
  lift =
    PerHref . pure . pure . pure . pure

instance MonadIO f => MonadIO (PerHref f) where
  liftIO =
    PerHref . pure . pure . pure . pure . liftIO

instance PerHref f a ~ x =>
  Rewrapped (PerHref g k) x

instance Wrapped (PerHref f k) where
  type Unwrapped (PerHref f k) =
      Href
      -> FilePath
      -> FilePath
      -> (String -> AipCon ())
      -> f k
  _Wrapped' =
    iso
      (\(PerHref x) -> x)
      PerHref

ioPerHref ::
  MonadIO f =>
  (Href -> FilePath -> FilePath -> (String -> AipCon ()) -> IO a)
  -> PerHref f a
ioPerHref k =
  PerHref (\h d d' l -> liftIO (k h d d' l))

nothingPerHref ::
  Applicative f =>
  PerHref f ()
nothingPerHref =
  pure ()

hrefPerHref ::
  Applicative f =>
  PerHref f Href
hrefPerHref =
  PerHref (\h _ _ _ -> pure h)

basedirPerHref ::
  Applicative f =>
  PerHref f FilePath
basedirPerHref =
  PerHref (\_ d _ _ -> pure d)

downloaddirPerHref ::
  Applicative f =>
  PerHref f FilePath
downloaddirPerHref =
  PerHref (\_ _ d' _ -> pure d')

logPerHref ::
  Applicative f =>
  PerHref f (String -> AipCon ())
logPerHref =
  PerHref (\_ _ _ l -> pure l)

type PerHrefAipCon a =
  PerHref AipCon a

type PerHrefIO a =
  PerHref IO a

logeachPerHref ::
  PerHrefAipCon ()
logeachPerHref =
  PerHref (\h d d' l ->
    let l' ::
          Show a =>
          a
          -> AipCon ()
        l' =
          l . show
    in  do  l' h
            l' d
            l' d')

logShowPerHref ::
  Show a =>
  a
  -> PerHrefAipCon ()
logShowPerHref z =
  do  l <- logPerHref
      lift (l (show z))

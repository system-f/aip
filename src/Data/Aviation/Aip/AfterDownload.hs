{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.AfterDownload(
  AfterDownload(..)
, nothingAfterDownload
, filePathAfterDownload
, hrefAfterDownload
, AfterDownloadAipCon
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
import System.FilePath(FilePath)

newtype AfterDownload f a =
  AfterDownload
    (FilePath -> Href -> f a)

instance Functor f => Functor (AfterDownload f) where
  fmap f (AfterDownload x) =
    AfterDownload (\p h -> fmap f (x p h))

instance Apply f => Apply (AfterDownload f) where
  AfterDownload f <.> AfterDownload a =
    AfterDownload (\p h -> f p h <.> a p h)

instance Applicative f => Applicative (AfterDownload f) where
  pure =
    AfterDownload . pure . pure . pure
  AfterDownload f <*> AfterDownload a =
    AfterDownload (\p h -> f p h <*> a p h)

instance Bind f => Bind (AfterDownload f) where
  AfterDownload x >>- f =
    AfterDownload (\p h -> x p h >>- \a -> let g = f a ^. _Wrapped in g p h)

instance Monad f => Monad (AfterDownload f) where
  return =
    pure
  AfterDownload x >>= f =
    AfterDownload (\p h -> x p h >>= \a -> let g = f a ^. _Wrapped in g p h)

instance Alt f => Alt (AfterDownload f) where
  AfterDownload x <!> AfterDownload y =
    AfterDownload (\p h -> x p h <!> y p h)

instance Alternative f => Alternative (AfterDownload f) where
  AfterDownload x <|> AfterDownload y =
    AfterDownload (\p h -> x p h <|> y p h)
  empty =
    (AfterDownload . pure . pure) empty

instance MonadTrans AfterDownload where
  lift =
    AfterDownload . pure . pure

instance MonadIO f => MonadIO (AfterDownload f) where
  liftIO =
    AfterDownload . pure . pure . liftIO

instance AfterDownload f a ~ x =>
  Rewrapped (AfterDownload g k) x

instance Wrapped (AfterDownload f k) where
  type Unwrapped (AfterDownload f k) =
      FilePath
      -> Href
      -> f k
  _Wrapped' =
    iso
      (\(AfterDownload x) -> x)
      AfterDownload

nothingAfterDownload ::
  Applicative f => AfterDownload f ()
nothingAfterDownload =
  pure ()

filePathAfterDownload ::
  Applicative f => AfterDownload f FilePath
filePathAfterDownload =
  AfterDownload (\p _ -> pure p)
  
hrefAfterDownload ::
  Applicative f => AfterDownload f Href
hrefAfterDownload =
  AfterDownload (\_ h -> pure h)

type AfterDownloadAipCon a =
  AfterDownload AipCon a

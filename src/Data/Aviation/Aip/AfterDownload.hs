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

import Data.Aviation.Aip.AipCon(AipCon)
import Data.Aviation.Aip.Href(Href)
import Papa

newtype AfterDownload f a =
  AfterDownload
    (FilePath -> Href -> f a)

instance Functor f => Functor (AfterDownload f) where
  fmap f (AfterDownload x) =
    AfterDownload (\p h -> fmap f (x p h))

instance Applicative f => Applicative (AfterDownload f) where
  pure =
    AfterDownload . pure . pure . pure
  AfterDownload f <*> AfterDownload a =
    AfterDownload (\p h -> f p h <*> a p h)

instance Monad f => Monad (AfterDownload f) where
  return =
    pure
  AfterDownload x >>= f =
    AfterDownload (\p h -> x p h >>= \a -> let g = f a ^. _Wrapped in g p h)

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

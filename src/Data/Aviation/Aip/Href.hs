{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Href(
  Href(..)
, dropHrefFile
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype Href =
  Href
    String
  deriving (Eq, Ord, Show)

instance FromJSON Href where
  parseJSON v =
    Href <$> parseJSON v

instance ToJSON Href where
  toJSON (Href x) =
    toJSON x

instance Semigroup Href where
  Href x <> Href y =
    Href (x <> y)

instance Monoid Href where
  mappend =
    (<>)
  mempty =
    Href mempty

instance Wrapped Href where
  type Unwrapped Href = String
  _Wrapped' =
    iso
      (\(Href x) -> x)
      Href

instance Href ~ a =>
  Rewrapped Href a

dropHrefFile ::
  Href
  -> Href
dropHrefFile =
  (_Wrapped %~ reverse . dropWhile (/= '/') . reverse)

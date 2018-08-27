{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.AipDate(
  AipDate(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype AipDate =
  AipDate
    String
  deriving (Eq, Ord, Show)

instance FromJSON AipDate where
  parseJSON v =
    AipDate <$> parseJSON v

instance ToJSON AipDate where
  toJSON (AipDate x) =
    toJSON x

instance Semigroup AipDate where
  AipDate x <> AipDate y =
    AipDate (x <> y)

instance Monoid AipDate where
  mappend =
    (<>)
  mempty =
    AipDate mempty

instance Wrapped AipDate where
  type Unwrapped AipDate = String
  _Wrapped' =
    iso
      (\(AipDate x) -> x)
      AipDate

instance AipDate ~ a =>
  Rewrapped AipDate a

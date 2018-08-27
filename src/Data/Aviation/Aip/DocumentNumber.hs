{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.DocumentNumber(
  DocumentNumber(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype DocumentNumber =
  DocumentNumber
    String
  deriving (Eq, Ord, Show)

instance FromJSON DocumentNumber where
  parseJSON v =
    DocumentNumber <$> parseJSON v

instance ToJSON DocumentNumber where
  toJSON (DocumentNumber x) =
    toJSON x

instance Semigroup DocumentNumber where
  DocumentNumber x <> DocumentNumber y =
    DocumentNumber (x <> y)

instance Monoid DocumentNumber where
  mappend =
    (<>)
  mempty =
    DocumentNumber mempty

instance Wrapped DocumentNumber where
  type Unwrapped DocumentNumber = String
  _Wrapped' =
    iso
      (\(DocumentNumber x) -> x)
      DocumentNumber

instance DocumentNumber ~ a =>
  Rewrapped DocumentNumber a

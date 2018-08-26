{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.DAPEntries(
  DAPEntries(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.DAPEntry(DAPEntry)
import Papa

newtype DAPEntries =
  DAPEntries
    [DAPEntry]
  deriving (Eq, Ord, Show)

instance Semigroup DAPEntries where
  DAPEntries x <> DAPEntries y =
    DAPEntries (x <> y)

instance Monoid DAPEntries where
  mappend =
    (<>)
  mempty =
    DAPEntries mempty

instance Wrapped DAPEntries where
  type Unwrapped DAPEntries = [DAPEntry]
  _Wrapped' =
    iso
      (\(DAPEntries x) -> x)
      DAPEntries

instance DAPEntries ~ a =>
  Rewrapped DAPEntries a

instance FromJSON DAPEntries where
  parseJSON =
    withArray "DAPEntries" $ \v ->
      DAPEntries <$> traverse parseJSON (toList v)

instance ToJSON DAPEntries where
  toJSON (DAPEntries x) =
    toJSON x

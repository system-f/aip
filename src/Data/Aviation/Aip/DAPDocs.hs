{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.DAPDocs(
  DAPDocs(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.DAPDoc(DAPDoc)
import Papa

newtype DAPDocs =
  DAPDocs
    [DAPDoc]
  deriving (Eq, Ord, Show)

instance Semigroup DAPDocs where
  DAPDocs x <> DAPDocs y =
    DAPDocs (x <> y)

instance Monoid DAPDocs where
  mappend =
    (<>)
  mempty =
    DAPDocs mempty

instance FromJSON DAPDocs where
  parseJSON =
    withArray "DAPDocs" $ \v ->
      DAPDocs <$> traverse parseJSON (toList v)

instance ToJSON DAPDocs where
  toJSON (DAPDocs x) =
    toJSON x

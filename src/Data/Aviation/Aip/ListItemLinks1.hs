{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.ListItemLinks1(
  ListItemLinks1(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.ListItemLink(ListItemLink)
import Papa

newtype ListItemLinks1 =
  ListItemLinks1
    [NonEmpty ListItemLink]
  deriving (Eq, Ord, Show)

instance Semigroup ListItemLinks1 where
  ListItemLinks1 x <> ListItemLinks1 y =
    ListItemLinks1 (x <> y)

instance Monoid ListItemLinks1 where
  mappend =
    (<>)
  mempty =
    ListItemLinks1 mempty

instance FromJSON ListItemLinks1 where
  parseJSON =
    withArray "ListItemLinks1" $ \v ->
      ListItemLinks1 <$> traverse parseJSON (toList v)

instance ToJSON ListItemLinks1 where
  toJSON (ListItemLinks1 x) =
    toJSON x

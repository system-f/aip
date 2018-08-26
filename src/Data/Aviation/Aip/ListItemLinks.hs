{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.ListItemLinks(
  ListItemLinks(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.ListItemLink(ListItemLink, ManyListItemLink(_ManyListItemLink))
import Papa

newtype ListItemLinks =
  ListItemLinks
    [ListItemLink]
  deriving (Eq, Ord, Show)

instance Semigroup ListItemLinks where
  ListItemLinks x <> ListItemLinks y =
    ListItemLinks (x <> y)

instance Monoid ListItemLinks where
  mappend =
    (<>)
  mempty =
    ListItemLinks []

instance ListItemLinks ~ x => Rewrapped ListItemLinks x

instance Wrapped ListItemLinks where
  type Unwrapped ListItemLinks =
    [ListItemLink]
  _Wrapped' =
    iso (\(ListItemLinks x) -> x) ListItemLinks

instance ManyListItemLink ListItemLinks where
  _ManyListItemLink f (ListItemLinks x) =
    ListItemLinks <$> traverse f x

instance FromJSON ListItemLinks where
  parseJSON =
    withArray "ListItemLinks" $ \v ->
      ListItemLinks <$> traverse parseJSON (toList v)

instance ToJSON ListItemLinks where
  toJSON (ListItemLinks x) =
    toJSON x

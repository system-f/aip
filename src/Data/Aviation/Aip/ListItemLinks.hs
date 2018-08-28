{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.ListItemLinks(
  ListItemLinks(..)
, AsListItemLinks(..)
, FoldListItemLinks(..)
, GetListItemLinks(..)
, SetListItemLinks(..)
, ManyListItemLinks(..)
, HasListItemLinks(..)
, IsListItemLinks(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.ListItemLink(ListItemLink, ManyListItemLink(_ManyListItemLink), SetListItemLink, FoldListItemLink(_FoldListItemLink))
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

instance Wrapped ListItemLinks where
  type Unwrapped ListItemLinks =
    [ListItemLink]
  _Wrapped' =
    iso (\(ListItemLinks x) -> x) ListItemLinks

instance ListItemLinks ~ x =>
  Rewrapped ListItemLinks x

instance FromJSON ListItemLinks where
  parseJSON =
    withArray "ListItemLinks" $ \v ->
      ListItemLinks <$> traverse parseJSON (toList v)

instance ToJSON ListItemLinks where
  toJSON (ListItemLinks x) =
    toJSON x

class AsListItemLinks a where
  _ListItemLinks ::
    Prism' a ListItemLinks
  default _ListItemLinks ::
    IsListItemLinks a =>
    Prism' a ListItemLinks
  _ListItemLinks =
    _IsListItemLinks
    
instance AsListItemLinks ListItemLinks where
  _ListItemLinks =
    id

class FoldListItemLinks a where
  _FoldListItemLinks ::
    Fold a ListItemLinks
    
instance FoldListItemLinks ListItemLinks where
  _FoldListItemLinks =
    id

class FoldListItemLinks a => GetListItemLinks a where
  _GetListItemLinks ::
    Getter a ListItemLinks
  default _GetListItemLinks ::
    HasListItemLinks a =>
    Getter a ListItemLinks
  _GetListItemLinks =
    listItemLinks
    
instance GetListItemLinks ListItemLinks where
  _GetListItemLinks =
    id

class SetListItemLinks a where
  _SetListItemLinks ::
    Setter' a ListItemLinks
  default _SetListItemLinks ::
    ManyListItemLinks a =>
    Setter' a ListItemLinks
  _SetListItemLinks =
    _ManyListItemLinks

instance SetListItemLinks ListItemLinks where
  _SetListItemLinks =
    id

class (FoldListItemLinks a, SetListItemLinks a) => ManyListItemLinks a where
  _ManyListItemLinks ::
    Traversal' a ListItemLinks

instance ManyListItemLinks ListItemLinks where
  _ManyListItemLinks =
    id

class (GetListItemLinks a, ManyListItemLinks a) => HasListItemLinks a where
  listItemLinks ::
    Lens' a ListItemLinks
  default listItemLinks ::
    IsListItemLinks a =>
    Lens' a ListItemLinks
  listItemLinks =
    _IsListItemLinks

instance HasListItemLinks ListItemLinks where
  listItemLinks =
    id

class (HasListItemLinks a, AsListItemLinks a) => IsListItemLinks a where
  _IsListItemLinks ::
    Iso' a ListItemLinks
    
instance IsListItemLinks ListItemLinks where
  _IsListItemLinks =
    id

----

instance SetListItemLink ListItemLinks where

instance FoldListItemLink ListItemLinks where
  _FoldListItemLink =
    _ManyListItemLink

instance ManyListItemLink ListItemLinks where
  _ManyListItemLink f (ListItemLinks x) =
    ListItemLinks <$> traverse f x

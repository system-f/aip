{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.ListItemLink(
  ListItemLink(..)
, AsListItemLink(..)
, FoldListItemLink(..)
, GetListItemLink(..)
, SetListItemLink(..)
, ManyListItemLink(..)
, HasListItemLink(..)
, IsListItemLink(..)
) where

import Data.Aviation.Aip.Href(Href)
import Data.Aviation.Aip.Txt(Txt)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Papa hiding ((.=))

data ListItemLink =
  ListItemLink
    Href
    Txt
  deriving (Eq, Ord, Show)

instance FromJSON ListItemLink where
  parseJSON =
    withObject "ListItemLink" $ \v ->
      ListItemLink <$>
        v .: "href" <*>
        v .: "txt" 

instance ToJSON ListItemLink where
  toJSON (ListItemLink u t) =
    object ["href" .= u, "txt" .= t]

class AsListItemLink a where
  _ListItemLink ::
    Prism' a ListItemLink
  default _ListItemLink ::
    IsListItemLink a =>
    Prism' a ListItemLink
  _ListItemLink =
    _IsListItemLink
    
instance AsListItemLink ListItemLink where
  _ListItemLink =
    id

class FoldListItemLink a where
  _FoldListItemLink ::
    Fold a ListItemLink
    
instance FoldListItemLink ListItemLink where
  _FoldListItemLink =
    id

class FoldListItemLink a => GetListItemLink a where
  _GetListItemLink ::
    Getter a ListItemLink
  default _GetListItemLink ::
    HasListItemLink a =>
    Getter a ListItemLink
  _GetListItemLink =
    listItemLink
    
instance GetListItemLink ListItemLink where
  _GetListItemLink =
    id

class SetListItemLink a where
  _SetListItemLink ::
    Setter' a ListItemLink
  default _SetListItemLink ::
    ManyListItemLink a =>
    Setter' a ListItemLink
  _SetListItemLink =
    _ManyListItemLink

instance SetListItemLink ListItemLink where
  _SetListItemLink =
    id

class (FoldListItemLink a, SetListItemLink a) => ManyListItemLink a where
  _ManyListItemLink ::
    Traversal' a ListItemLink

instance ManyListItemLink ListItemLink where
  _ManyListItemLink =
    id

class (GetListItemLink a, ManyListItemLink a) => HasListItemLink a where
  listItemLink ::
    Lens' a ListItemLink
  default listItemLink ::
    IsListItemLink a =>
    Lens' a ListItemLink
  listItemLink =
    _IsListItemLink

instance HasListItemLink ListItemLink where
  listItemLink =
    id

class (HasListItemLink a, AsListItemLink a) => IsListItemLink a where
  _IsListItemLink ::
    Iso' a ListItemLink
    
instance IsListItemLink ListItemLink where
  _IsListItemLink =
    id
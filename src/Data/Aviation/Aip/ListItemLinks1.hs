{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.ListItemLinks1(
  ListItemLinks1(..)
, AsListItemLinks1(..)
, FoldListItemLinks1(..)
, GetListItemLinks1(..)
, SetListItemLinks1(..)
, ManyListItemLinks1(..)
, HasListItemLinks1(..)
, IsListItemLinks1(..)  
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

instance Wrapped ListItemLinks1 where
  type Unwrapped ListItemLinks1 =
    [NonEmpty ListItemLink]
  _Wrapped' =
    iso (\(ListItemLinks1 x) -> x) ListItemLinks1

instance ListItemLinks1 ~ x =>
  Rewrapped ListItemLinks1 x

instance Cons ListItemLinks1 ListItemLinks1 (NonEmpty ListItemLink) (NonEmpty ListItemLink) where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc ListItemLinks1 ListItemLinks1 (NonEmpty ListItemLink) (NonEmpty ListItemLink) where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each ListItemLinks1 ListItemLinks1 (NonEmpty ListItemLink) (NonEmpty ListItemLink) where
  each =
    _Wrapped . each

instance Reversing ListItemLinks1 where
  reversing =
    _Wrapped %~ reversing

instance Plated ListItemLinks1 where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue ListItemLinks1 = NonEmpty ListItemLink
type instance Index ListItemLinks1 = Int
instance Ixed ListItemLinks1 where
  ix i =
    _Wrapped . ix i

class AsListItemLinks1 a where
  _ListItemLinks1 ::
    Prism' a ListItemLinks1
  default _ListItemLinks1 ::
    IsListItemLinks1 a =>
    Prism' a ListItemLinks1
  _ListItemLinks1 =
    _IsListItemLinks1
    
instance AsListItemLinks1 ListItemLinks1 where
  _ListItemLinks1 =
    id

class FoldListItemLinks1 a where
  _FoldListItemLinks1 ::
    Fold a ListItemLinks1
    
instance FoldListItemLinks1 ListItemLinks1 where
  _FoldListItemLinks1 =
    id

class FoldListItemLinks1 a => GetListItemLinks1 a where
  _GetListItemLinks1 ::
    Getter a ListItemLinks1
  default _GetListItemLinks1 ::
    HasListItemLinks1 a =>
    Getter a ListItemLinks1
  _GetListItemLinks1 =
    listItemLinks1
    
instance GetListItemLinks1 ListItemLinks1 where
  _GetListItemLinks1 =
    id

class SetListItemLinks1 a where
  _SetListItemLinks1 ::
    Setter' a ListItemLinks1
  default _SetListItemLinks1 ::
    ManyListItemLinks1 a =>
    Setter' a ListItemLinks1
  _SetListItemLinks1 =
    _ManyListItemLinks1

instance SetListItemLinks1 ListItemLinks1 where
  _SetListItemLinks1 =
    id

class (FoldListItemLinks1 a, SetListItemLinks1 a) => ManyListItemLinks1 a where
  _ManyListItemLinks1 ::
    Traversal' a ListItemLinks1

instance ManyListItemLinks1 ListItemLinks1 where
  _ManyListItemLinks1 =
    id

class (GetListItemLinks1 a, ManyListItemLinks1 a) => HasListItemLinks1 a where
  listItemLinks1 ::
    Lens' a ListItemLinks1
  default listItemLinks1 ::
    IsListItemLinks1 a =>
    Lens' a ListItemLinks1
  listItemLinks1 =
    _IsListItemLinks1

instance HasListItemLinks1 ListItemLinks1 where
  listItemLinks1 =
    id

class (HasListItemLinks1 a, AsListItemLinks1 a) => IsListItemLinks1 a where
  _IsListItemLinks1 ::
    Iso' a ListItemLinks1
    
instance IsListItemLinks1 ListItemLinks1 where
  _IsListItemLinks1 =
    id

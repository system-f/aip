{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.DAPEntries(
  DAPEntries(..)
, AsDAPEntries(..)
, FoldDAPEntries(..)
, GetDAPEntries(..)
, SetDAPEntries(..)
, ManyDAPEntries(..)
, HasDAPEntries(..)
, IsDAPEntries(..)
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

instance Cons DAPEntries DAPEntries DAPEntry DAPEntry where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc DAPEntries DAPEntries DAPEntry DAPEntry where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each DAPEntries DAPEntries DAPEntry DAPEntry where
  each =
    _Wrapped . each

instance Reversing DAPEntries where
  reversing =
    _Wrapped %~ reversing

instance Plated DAPEntries where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue DAPEntries = DAPEntry
type instance Index DAPEntries = Int
instance Ixed DAPEntries where
  ix i =
    _Wrapped . ix i

class AsDAPEntries a where
  _DAPEntries ::
    Prism' a DAPEntries
  default _DAPEntries ::
    IsDAPEntries a =>
    Prism' a DAPEntries
  _DAPEntries =
    _IsDAPEntries
    
instance AsDAPEntries DAPEntries where
  _DAPEntries =
    id

class FoldDAPEntries a where
  _FoldDAPEntries ::
    Fold a DAPEntries
    
instance FoldDAPEntries DAPEntries where
  _FoldDAPEntries =
    id

class FoldDAPEntries a => GetDAPEntries a where
  _GetDAPEntries ::
    Getter a DAPEntries
  default _GetDAPEntries ::
    HasDAPEntries a =>
    Getter a DAPEntries
  _GetDAPEntries =
    dapEntries
    
instance GetDAPEntries DAPEntries where
  _GetDAPEntries =
    id

class SetDAPEntries a where
  _SetDAPEntries ::
    Setter' a DAPEntries
  default _SetDAPEntries ::
    ManyDAPEntries a =>
    Setter' a DAPEntries
  _SetDAPEntries =
    _ManyDAPEntries

instance SetDAPEntries DAPEntries where
  _SetDAPEntries =
    id

class (FoldDAPEntries a, SetDAPEntries a) => ManyDAPEntries a where
  _ManyDAPEntries ::
    Traversal' a DAPEntries

instance ManyDAPEntries DAPEntries where
  _ManyDAPEntries =
    id

class (GetDAPEntries a, ManyDAPEntries a) => HasDAPEntries a where
  dapEntries ::
    Lens' a DAPEntries
  default dapEntries ::
    IsDAPEntries a =>
    Lens' a DAPEntries
  dapEntries =
    _IsDAPEntries

instance HasDAPEntries DAPEntries where
  dapEntries =
    id

class (HasDAPEntries a, AsDAPEntries a) => IsDAPEntries a where
  _IsDAPEntries ::
    Iso' a DAPEntries
    
instance IsDAPEntries DAPEntries where
  _IsDAPEntries =
    id

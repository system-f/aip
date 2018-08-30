{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.DAPDoc(
  DAPDoc(..)
, AsDAPDoc(..)
, FoldDAPDoc(..)
, GetDAPDoc(..)
, SetDAPDoc(..)
, ManyDAPDoc(..)
, HasDAPDoc(..)
, IsDAPDoc(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.DAPType(DAPType)
import Data.Aviation.Aip.DAPEntries(DAPEntries)
import Data.Aviation.Aip.Href(Href)
import Papa hiding ((.=))

data DAPDoc =
  DAPDoc
    (DAPType String)
    Href
    DAPEntries
  deriving (Eq, Ord, Show)

instance FromJSON DAPDoc where
  parseJSON =
    withObject "DAPDoc" $ \v ->
      DAPDoc <$>
        v .: "type" <*>
        v .: "href" <*>
        v .: "entries"

instance ToJSON DAPDoc where
  toJSON (DAPDoc typ href entries) =
    object ["type" .= typ, "href" .= href, "entries" .= entries]

class AsDAPDoc a where
  _DAPDoc ::
    Prism' a DAPDoc
  default _DAPDoc ::
    IsDAPDoc a =>
    Prism' a DAPDoc
  _DAPDoc =
    _IsDAPDoc
    
instance AsDAPDoc DAPDoc where
  _DAPDoc =
    id

class FoldDAPDoc a where
  _FoldDAPDoc ::
    Fold a DAPDoc
    
instance FoldDAPDoc DAPDoc where
  _FoldDAPDoc =
    id

class FoldDAPDoc a => GetDAPDoc a where
  _GetDAPDoc ::
    Getter a DAPDoc
  default _GetDAPDoc ::
    HasDAPDoc a =>
    Getter a DAPDoc
  _GetDAPDoc =
    dapDoc
    
instance GetDAPDoc DAPDoc where
  _GetDAPDoc =
    id

class SetDAPDoc a where
  _SetDAPDoc ::
    Setter' a DAPDoc
  default _SetDAPDoc ::
    ManyDAPDoc a =>
    Setter' a DAPDoc
  _SetDAPDoc =
    _ManyDAPDoc

instance SetDAPDoc DAPDoc where
  _SetDAPDoc =
    id

class (FoldDAPDoc a, SetDAPDoc a) => ManyDAPDoc a where
  _ManyDAPDoc ::
    Traversal' a DAPDoc

instance ManyDAPDoc DAPDoc where
  _ManyDAPDoc =
    id

class (GetDAPDoc a, ManyDAPDoc a) => HasDAPDoc a where
  dapDoc ::
    Lens' a DAPDoc
  default dapDoc ::
    IsDAPDoc a =>
    Lens' a DAPDoc
  dapDoc =
    _IsDAPDoc

instance HasDAPDoc DAPDoc where
  dapDoc =
    id

class (HasDAPDoc a, AsDAPDoc a) => IsDAPDoc a where
  _IsDAPDoc ::
    Iso' a DAPDoc
    
instance IsDAPDoc DAPDoc where
  _IsDAPDoc =
    id

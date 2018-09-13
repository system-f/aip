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

import Control.Category(id)
import Control.Applicative(pure, (<*>))
import Control.Lens hiding ((.=))
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.DAPType(DAPType)
import Data.Aviation.Aip.DAPEntries(DAPEntries, FoldDAPEntries(_FoldDAPEntries), GetDAPEntries, SetDAPEntries, ManyDAPEntries(_ManyDAPEntries), HasDAPEntries(dapEntries))
import Data.Aviation.Aip.Href(Href, SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Data.Eq(Eq)
import Data.Function(($))
import Data.Functor(fmap, (<$>))
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

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
  toJSON (DAPDoc typ u entries) =
    object ["type" .= typ, "href" .= u, "entries" .= entries]

class ManyDAPDoc a => AsDAPDoc a where
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

instance SetDAPDoc () where
instance FoldDAPDoc () where
  _FoldDAPDoc =
    _ManyDAPDoc
instance ManyDAPDoc () where
  _ManyDAPDoc _ x =
    pure x

instance SetHref DAPDoc where
instance FoldHref DAPDoc where
  _FoldHref =
    _ManyHref

instance ManyHref DAPDoc where
  _ManyHref f (DAPDoc typ u entries) =
    DAPDoc <$> pure typ <*> f u <*> _ManyHref f entries

----

instance FoldDAPEntries DAPDoc where
  _FoldDAPEntries =
    dapEntries
instance GetDAPEntries DAPDoc where
instance SetDAPEntries DAPDoc where
instance ManyDAPEntries DAPDoc where
  _ManyDAPEntries =
    dapEntries
instance HasDAPEntries DAPDoc where
  dapEntries k (DAPDoc typ u entries) =
    fmap (\entries' -> DAPDoc typ u entries') (k entries)

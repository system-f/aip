{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.AipRecord(
  AipRecord(..)
, AsAipRecord(..)
, FoldAipRecord(..)
, GetAipRecord(..)
, SetAipRecord(..)
, ManyAipRecord(..)
, HasAipRecord(..)
, IsAipRecord(..)    
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.Aip_SUP_and_AICs(Aip_SUP_and_AICs)
import Data.Aviation.Aip.AipDocuments(AipDocuments)
import Data.Aviation.Aip.DAPDocs(DAPDocs)
import Data.Aviation.Aip.Ersa(Ersa)
import Data.Aviation.Aip.Href(SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ListItemLinks1(ListItemLinks1)
import Data.Time(UTCTime)
import Papa hiding ((.=))

data AipRecord =
  AipRecord
    UTCTime
    (AipDocuments ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa)
  deriving (Eq, Show)

instance FromJSON AipRecord where
  parseJSON =
    withObject "AipRecord" $ \v ->
      AipRecord <$>
        v .: "utc" <*>
        v .: "documents"

instance ToJSON AipRecord where
  toJSON (AipRecord t p) =
    object ["utc" .= t, "documents" .= p]

class AsAipRecord a where
  _AipRecord ::
    Prism' a AipRecord
  default _AipRecord ::
    IsAipRecord a =>
    Prism' a AipRecord
  _AipRecord =
    _IsAipRecord
    
instance AsAipRecord AipRecord where
  _AipRecord =
    id

class FoldAipRecord a where
  _FoldAipRecord ::
    Fold a AipRecord
    
instance FoldAipRecord AipRecord where
  _FoldAipRecord =
    id

class FoldAipRecord a => GetAipRecord a where
  _GetAipRecord ::
    Getter a AipRecord
  default _GetAipRecord ::
    HasAipRecord a =>
    Getter a AipRecord
  _GetAipRecord =
    aipRecord
    
instance GetAipRecord AipRecord where
  _GetAipRecord =
    id

class SetAipRecord a where
  _SetAipRecord ::
    Setter' a AipRecord
  default _SetAipRecord ::
    ManyAipRecord a =>
    Setter' a AipRecord
  _SetAipRecord =
    _ManyAipRecord

instance SetAipRecord AipRecord where
  _SetAipRecord =
    id

class (FoldAipRecord a, SetAipRecord a) => ManyAipRecord a where
  _ManyAipRecord ::
    Traversal' a AipRecord

instance ManyAipRecord AipRecord where
  _ManyAipRecord =
    id

class (GetAipRecord a, ManyAipRecord a) => HasAipRecord a where
  aipRecord ::
    Lens' a AipRecord
  default aipRecord ::
    IsAipRecord a =>
    Lens' a AipRecord
  aipRecord =
    _IsAipRecord

instance HasAipRecord AipRecord where
  aipRecord =
    id

class (HasAipRecord a, AsAipRecord a) => IsAipRecord a where
  _IsAipRecord ::
    Iso' a AipRecord
    
instance IsAipRecord AipRecord where
  _IsAipRecord =
    id

instance SetAipRecord () where
instance FoldAipRecord () where
  _FoldAipRecord =
    _ManyAipRecord
instance ManyAipRecord () where
  _ManyAipRecord _ x =
    pure x

----

instance SetHref AipRecord where
instance FoldHref AipRecord where
  _FoldHref =
    _ManyHref

instance ManyHref AipRecord where
  _ManyHref f (AipRecord t p) =
    AipRecord <$> pure t <*> _ManyHref f p

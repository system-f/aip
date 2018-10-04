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

import Control.Category(id)
import Control.Applicative(pure, (<*>))
import Control.Lens hiding ((.=))
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.AipDocuments(AipDocuments2)
import Data.Aviation.Aip.Href(Href, SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Data.Eq(Eq)
import Data.Function(($))
import Data.Functor(fmap, (<$>))
import Data.Time(UTCTime)
import Prelude(Show)

data AipRecord =
  AipRecord
    UTCTime
    Href
    AipDocuments2
  deriving (Eq, Show)

instance FromJSON AipRecord where
  parseJSON =
    withObject "AipRecord" $ \v ->
      AipRecord <$>
        v .: "utc" <*>
        v .: "index" <*>
        v .: "documents"

instance ToJSON AipRecord where
  toJSON (AipRecord t i p) =
    object ["utc" .= t, "index" .= i, "documents" .= p]

class ManyAipRecord a => AsAipRecord a where
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
  aipRecordTime ::
    Lens' a UTCTime
  aipRecordIndex ::
    Lens' a Href
  aipRecordAipDocuments ::
    Lens' a AipDocuments2

instance HasAipRecord AipRecord where
  aipRecord =
    id
  aipRecordTime k (AipRecord t i p) =
    fmap (\t' -> AipRecord t' i p) (k t)
  aipRecordIndex k (AipRecord t i p) =
    fmap (\i' -> AipRecord t i' p) (k i)
  aipRecordAipDocuments k (AipRecord t i p) =
    fmap (\p' -> AipRecord t i p') (k p)

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
  _ManyHref f (AipRecord t i p) =
    AipRecord <$> pure t <*> _ManyHref f i <*> _ManyHref f p

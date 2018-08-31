{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.ErsaAerodrome(
  ErsaAerodrome(..)
, AsErsaAerodrome(..)
, FoldErsaAerodrome(..)
, GetErsaAerodrome(..)
, SetErsaAerodrome(..)
, ManyErsaAerodrome(..)
, HasErsaAerodrome(..)
, IsErsaAerodrome(..)    
) where

import Data.Aviation.Aip.Href(Href, SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Papa hiding ((.=))

data ErsaAerodrome =
  ErsaAerodrome
    String
    Href
    (Maybe Href)
  deriving (Eq, Ord, Show)

instance FromJSON ErsaAerodrome where
  parseJSON =
    withObject "ErsaAerodrome" $ \v ->
      ErsaAerodrome <$>
        v .: "aerodrome" <*>
        v .: "fac_href" <*>
        v .: "rds_href"

instance ToJSON ErsaAerodrome where
  toJSON (ErsaAerodrome aerodrome fac rds) =
    object ["aerodrome" .= aerodrome, "fac_href" .= fac, "rds_href" .= rds]

class AsErsaAerodrome a where
  _ErsaAerodrome ::
    Prism' a ErsaAerodrome
  default _ErsaAerodrome ::
    IsErsaAerodrome a =>
    Prism' a ErsaAerodrome
  _ErsaAerodrome =
    _IsErsaAerodrome
    
instance AsErsaAerodrome ErsaAerodrome where
  _ErsaAerodrome =
    id

class FoldErsaAerodrome a where
  _FoldErsaAerodrome ::
    Fold a ErsaAerodrome
    
instance FoldErsaAerodrome ErsaAerodrome where
  _FoldErsaAerodrome =
    id

class FoldErsaAerodrome a => GetErsaAerodrome a where
  _GetErsaAerodrome ::
    Getter a ErsaAerodrome
  default _GetErsaAerodrome ::
    HasErsaAerodrome a =>
    Getter a ErsaAerodrome
  _GetErsaAerodrome =
    ersaAerodrome
    
instance GetErsaAerodrome ErsaAerodrome where
  _GetErsaAerodrome =
    id

class SetErsaAerodrome a where
  _SetErsaAerodrome ::
    Setter' a ErsaAerodrome
  default _SetErsaAerodrome ::
    ManyErsaAerodrome a =>
    Setter' a ErsaAerodrome
  _SetErsaAerodrome =
    _ManyErsaAerodrome

instance SetErsaAerodrome ErsaAerodrome where
  _SetErsaAerodrome =
    id

class (FoldErsaAerodrome a, SetErsaAerodrome a) => ManyErsaAerodrome a where
  _ManyErsaAerodrome ::
    Traversal' a ErsaAerodrome

instance ManyErsaAerodrome ErsaAerodrome where
  _ManyErsaAerodrome =
    id

class (GetErsaAerodrome a, ManyErsaAerodrome a) => HasErsaAerodrome a where
  ersaAerodrome ::
    Lens' a ErsaAerodrome
  default ersaAerodrome ::
    IsErsaAerodrome a =>
    Lens' a ErsaAerodrome
  ersaAerodrome =
    _IsErsaAerodrome

instance HasErsaAerodrome ErsaAerodrome where
  ersaAerodrome =
    id

class (HasErsaAerodrome a, AsErsaAerodrome a) => IsErsaAerodrome a where
  _IsErsaAerodrome ::
    Iso' a ErsaAerodrome
    
instance IsErsaAerodrome ErsaAerodrome where
  _IsErsaAerodrome =
    id

instance SetErsaAerodrome () where
instance FoldErsaAerodrome () where
  _FoldErsaAerodrome =
    _ManyErsaAerodrome
instance ManyErsaAerodrome () where
  _ManyErsaAerodrome _ x =
    pure x

----

instance SetHref ErsaAerodrome where
instance FoldHref ErsaAerodrome where
  _FoldHref =
    _ManyHref

instance ManyHref ErsaAerodrome where
  _ManyHref f (ErsaAerodrome aerodrome fac rds) =
    ErsaAerodrome <$> pure aerodrome <*> f fac <*> traverse f rds


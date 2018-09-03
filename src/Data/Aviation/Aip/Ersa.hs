{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.Ersa(
  Ersa(..)
, AsErsa(..)
, FoldErsa(..)
, GetErsa(..)
, SetErsa(..)
, ManyErsa(..)
, HasErsa(..)
, IsErsa(..)    
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.Href(Href, SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ErsaAerodromes(ErsaAerodromes)
import Papa hiding ((.=))

data Ersa =
  Ersa
    ListItemLinks
    ErsaAerodromes
    [Href] -- complete ERSA
  deriving (Eq, Ord, Show)
  
instance Semigroup Ersa where
  Ersa l1 a1 c1 <> Ersa l2 a2 c2 =
    Ersa (l1 <> l2) (a1 <> a2) (c1 <> c2)

instance Monoid Ersa where
  mappend =
    (<>)
  mempty =
    Ersa mempty mempty mempty

instance FromJSON Ersa where
  parseJSON =
    withObject "Ersa" $ \v ->
      Ersa <$>
        v .: "links" <*>
        v .: "aerodromes" <*>
        v .: "complete"

instance ToJSON Ersa where
  toJSON (Ersa links aerodromes complete) =
    object ["links" .= links, "aerodromes" .= aerodromes, "complete" .= complete]

class AsErsa a where
  _Ersa ::
    Prism' a Ersa
  default _Ersa ::
    IsErsa a =>
    Prism' a Ersa
  _Ersa =
    _IsErsa
    
instance AsErsa Ersa where
  _Ersa =
    id

class FoldErsa a where
  _FoldErsa ::
    Fold a Ersa
    
instance FoldErsa Ersa where
  _FoldErsa =
    id

class FoldErsa a => GetErsa a where
  _GetErsa ::
    Getter a Ersa
  default _GetErsa ::
    HasErsa a =>
    Getter a Ersa
  _GetErsa =
    ersa
    
instance GetErsa Ersa where
  _GetErsa =
    id

class SetErsa a where
  _SetErsa ::
    Setter' a Ersa
  default _SetErsa ::
    ManyErsa a =>
    Setter' a Ersa
  _SetErsa =
    _ManyErsa

instance SetErsa Ersa where
  _SetErsa =
    id

class (FoldErsa a, SetErsa a) => ManyErsa a where
  _ManyErsa ::
    Traversal' a Ersa

instance ManyErsa Ersa where
  _ManyErsa =
    id

class (GetErsa a, ManyErsa a) => HasErsa a where
  ersa ::
    Lens' a Ersa
  default ersa ::
    IsErsa a =>
    Lens' a Ersa
  ersa =
    _IsErsa

instance HasErsa Ersa where
  ersa =
    id

class (HasErsa a, AsErsa a) => IsErsa a where
  _IsErsa ::
    Iso' a Ersa
    
instance IsErsa Ersa where
  _IsErsa =
    id

instance SetErsa () where
instance FoldErsa () where
  _FoldErsa =
    _ManyErsa
instance ManyErsa () where
  _ManyErsa _ x =
    pure x

----

instance SetHref Ersa where
instance FoldHref Ersa where
  _FoldHref =
    _ManyHref

instance ManyHref Ersa where
  _ManyHref f (Ersa l a c) =
    Ersa <$> _ManyHref f l <*> _ManyHref f a <*> traverse f c

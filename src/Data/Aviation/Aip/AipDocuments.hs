{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.AipDocuments(
  AipDocuments(..)
, AipDocuments1
, AipDocuments2
, AsAipDocuments(..)
, FoldAipDocuments(..)
, GetAipDocuments(..)
, SetAipDocuments(..)
, ManyAipDocuments(..)
, HasAipDocuments(..)
, IsAipDocuments(..)    
) where

import Control.Category((.), id)
import Control.Lens
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.Aip_SUP_and_AICs(Aip_SUP_and_AICs)
import Data.Aviation.Aip.AipDocument(AipDocument)
import Data.Aviation.Aip.DAPDocs(DAPDocs)
import Data.Aviation.Aip.Ersa(Ersa)
import Data.Aviation.Aip.Href(SetHref, FoldHref(_FoldHref), ManyHref(_ManyHref))
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ListItemLinks1(ListItemLinks1)
import Data.Eq(Eq)
import Data.Foldable(toList)
import Data.Function(($))
import Data.Functor((<$>))
import Data.Int(Int)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Prelude(Show)

newtype AipDocuments book charts sup_aic dap ersa =
  AipDocuments
    [AipDocument book charts sup_aic dap ersa]
  deriving (Eq, Ord, Show)

instance Semigroup (AipDocuments book charts sup_aic dap ersa) where
  AipDocuments x <> AipDocuments y =
    AipDocuments (x <> y)

instance Monoid (AipDocuments book charts sup_aic dap ersa) where
  mappend =
    (<>)
  mempty =
    AipDocuments []

instance Wrapped (AipDocuments book charts sup_aic dap ersa) where
  type Unwrapped (AipDocuments book charts sup_aic dap ersa) =
    [AipDocument book charts sup_aic dap ersa]
  _Wrapped' =
    iso (\(AipDocuments x) -> x) AipDocuments

instance (AipDocuments book charts sup_aic dap ersa) ~ x =>
  Rewrapped (AipDocuments book charts sup_aic dap ersa) x

type AipDocuments1 =
  AipDocuments () () () () ()

type AipDocuments2 =
  AipDocuments ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa

instance (FromJSON book, FromJSON charts, FromJSON sup_aic, FromJSON dap, FromJSON ersa) => FromJSON (AipDocuments book charts sup_aic dap ersa) where
  parseJSON =
    withArray "AipDocuments" $ \v ->
      AipDocuments <$> traverse parseJSON (toList v)

instance (ToJSON book, ToJSON charts, ToJSON sup_aic, ToJSON dap, ToJSON ersa) => ToJSON (AipDocuments book charts sup_aic dap ersa) where
  toJSON (AipDocuments x) =
    toJSON x

instance Cons (AipDocuments book charts sup_aic dap ersa) (AipDocuments book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa) where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc (AipDocuments book charts sup_aic dap ersa) (AipDocuments book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa) where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each (AipDocuments book charts sup_aic dap ersa) (AipDocuments book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa) where
  each =
    _Wrapped . each

instance Reversing (AipDocuments book charts sup_aic dap ersa) where
  reversing =
    _Wrapped %~ reversing

instance Plated (AipDocuments book charts sup_aic dap ersa) where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue (AipDocuments book charts sup_aic dap ersa) = (AipDocument book charts sup_aic dap ersa)
type instance Index (AipDocuments book charts sup_aic dap ersa) = Int
instance Ixed (AipDocuments book charts sup_aic dap ersa) where
  ix i =
    _Wrapped . ix i

class ManyAipDocuments a => AsAipDocuments a where
  _AipDocuments ::
    Prism (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
  default _AipDocuments ::
    IsAipDocuments a =>
    Prism (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
  _AipDocuments =
    _IsAipDocuments

instance AsAipDocuments AipDocuments where
  _AipDocuments =
    id

class FoldAipDocuments a where
  _FoldAipDocuments ::
    Fold (a book charts sup_aic dap ersa) (AipDocuments book charts sup_aic dap ersa)
    
instance FoldAipDocuments AipDocuments where
  _FoldAipDocuments =
    id

class FoldAipDocuments a => GetAipDocuments a where
  _GetAipDocuments ::
    Getter (a book charts sup_aic dap ersa) (AipDocuments book charts sup_aic dap ersa)
  default _GetAipDocuments ::
    HasAipDocuments a =>
    Getter (a book charts sup_aic dap ersa) (AipDocuments book charts sup_aic dap ersa)
  _GetAipDocuments =
    aipDocuments
    
instance GetAipDocuments AipDocuments where
  _GetAipDocuments =
    id

class SetAipDocuments a where
  _SetAipDocuments ::
    Setter (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
  default _SetAipDocuments ::
    ManyAipDocuments a =>
    Setter (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
  _SetAipDocuments =
    _ManyAipDocuments

instance SetAipDocuments AipDocuments where
  _SetAipDocuments =
    id

class (FoldAipDocuments a, SetAipDocuments a) => ManyAipDocuments a where
  _ManyAipDocuments ::
    Traversal (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')

instance ManyAipDocuments AipDocuments where
  _ManyAipDocuments =
    id

class (GetAipDocuments a, ManyAipDocuments a) => HasAipDocuments a where
  aipDocuments ::
    Lens (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
  default aipDocuments ::
    IsAipDocuments a =>
    Lens (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
  aipDocuments =
    _IsAipDocuments

instance HasAipDocuments AipDocuments where
  aipDocuments =
    id

class (HasAipDocuments a, AsAipDocuments a) => IsAipDocuments a where
  _IsAipDocuments ::
    Iso (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocuments book charts sup_aic dap ersa) (AipDocuments book' charts' sup_aic' dap' ersa')
    
instance IsAipDocuments AipDocuments where
  _IsAipDocuments =
    id

----

instance (ManyHref book, ManyHref charts, ManyHref sup_aic, ManyHref dap, ManyHref ersa) => SetHref (AipDocuments book charts sup_aic dap ersa) where
instance (ManyHref book, ManyHref charts, ManyHref sup_aic, ManyHref dap, ManyHref ersa) => FoldHref (AipDocuments book charts sup_aic dap ersa) where
  _FoldHref =
    _ManyHref

instance (ManyHref book, ManyHref charts, ManyHref sup_aic, ManyHref dap, ManyHref ersa) => ManyHref (AipDocuments book charts sup_aic dap ersa) where
  _ManyHref =
    _Wrapped . traverse . _ManyHref

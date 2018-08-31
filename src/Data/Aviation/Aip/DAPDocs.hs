{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.DAPDocs(
  DAPDocs(..)
, AsDAPDocs(..)
, FoldDAPDocs(..)
, GetDAPDocs(..)
, SetDAPDocs(..)
, ManyDAPDocs(..)
, HasDAPDocs(..)
, IsDAPDocs(..)  
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.DAPDoc(DAPDoc)
import Data.Aviation.Aip.Href(SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Papa

newtype DAPDocs =
  DAPDocs
    [DAPDoc]
  deriving (Eq, Ord, Show)

instance Semigroup DAPDocs where
  DAPDocs x <> DAPDocs y =
    DAPDocs (x <> y)

instance Monoid DAPDocs where
  mappend =
    (<>)
  mempty =
    DAPDocs mempty

instance FromJSON DAPDocs where
  parseJSON =
    withArray "DAPDocs" $ \v ->
      DAPDocs <$> traverse parseJSON (toList v)

instance ToJSON DAPDocs where
  toJSON (DAPDocs x) =
    toJSON x

instance Wrapped DAPDocs where
  type Unwrapped DAPDocs =
    [DAPDoc]
  _Wrapped' =
    iso (\(DAPDocs x) -> x) DAPDocs

instance DAPDocs ~ x =>
  Rewrapped DAPDocs x

instance Cons DAPDocs DAPDocs DAPDoc DAPDoc where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc DAPDocs DAPDocs DAPDoc DAPDoc where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each DAPDocs DAPDocs DAPDoc DAPDoc where
  each =
    _Wrapped . each

instance Reversing DAPDocs where
  reversing =
    _Wrapped %~ reversing

instance Plated DAPDocs where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue DAPDocs = DAPDoc
type instance Index DAPDocs = Int
instance Ixed DAPDocs where
  ix i =
    _Wrapped . ix i

class AsDAPDocs a where
  _DAPDocs ::
    Prism' a DAPDocs
  default _DAPDocs ::
    IsDAPDocs a =>
    Prism' a DAPDocs
  _DAPDocs =
    _IsDAPDocs
    
instance AsDAPDocs DAPDocs where
  _DAPDocs =
    id

class FoldDAPDocs a where
  _FoldDAPDocs ::
    Fold a DAPDocs
    
instance FoldDAPDocs DAPDocs where
  _FoldDAPDocs =
    id

class FoldDAPDocs a => GetDAPDocs a where
  _GetDAPDocs ::
    Getter a DAPDocs
  default _GetDAPDocs ::
    HasDAPDocs a =>
    Getter a DAPDocs
  _GetDAPDocs =
    dapDocs
    
instance GetDAPDocs DAPDocs where
  _GetDAPDocs =
    id

class SetDAPDocs a where
  _SetDAPDocs ::
    Setter' a DAPDocs
  default _SetDAPDocs ::
    ManyDAPDocs a =>
    Setter' a DAPDocs
  _SetDAPDocs =
    _ManyDAPDocs

instance SetDAPDocs DAPDocs where
  _SetDAPDocs =
    id

class (FoldDAPDocs a, SetDAPDocs a) => ManyDAPDocs a where
  _ManyDAPDocs ::
    Traversal' a DAPDocs

instance ManyDAPDocs DAPDocs where
  _ManyDAPDocs =
    id

class (GetDAPDocs a, ManyDAPDocs a) => HasDAPDocs a where
  dapDocs ::
    Lens' a DAPDocs
  default dapDocs ::
    IsDAPDocs a =>
    Lens' a DAPDocs
  dapDocs =
    _IsDAPDocs

instance HasDAPDocs DAPDocs where
  dapDocs =
    id

class (HasDAPDocs a, AsDAPDocs a) => IsDAPDocs a where
  _IsDAPDocs ::
    Iso' a DAPDocs
    
instance IsDAPDocs DAPDocs where
  _IsDAPDocs =
    id

instance SetDAPDocs () where
instance FoldDAPDocs () where
  _FoldDAPDocs =
    _ManyDAPDocs
instance ManyDAPDocs () where
  _ManyDAPDocs _ x =
    pure x

instance SetHref DAPDocs where
instance FoldHref DAPDocs where
  _FoldHref =
    _ManyHref

instance ManyHref DAPDocs where
  _ManyHref =
    _Wrapped . traverse . _ManyHref

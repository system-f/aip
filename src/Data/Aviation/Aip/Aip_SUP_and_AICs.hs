{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Aip_SUP_and_AICs(
  Aip_SUP_and_AICs(..)
, AsAip_SUP_and_AICs(..)
, FoldAip_SUP_and_AICs(..)
, GetAip_SUP_and_AICs(..)
, SetAip_SUP_and_AICs(..)
, ManyAip_SUP_and_AICs(..)
, HasAip_SUP_and_AICs(..)
, IsAip_SUP_and_AICs(..)    
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.Href(SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Data.Aviation.Aip.Aip_SUP_and_AIC(Aip_SUP_and_AIC)
import Papa

newtype Aip_SUP_and_AICs =
  Aip_SUP_and_AICs
    [Aip_SUP_and_AIC]
  deriving (Eq, Ord, Show)

instance Semigroup Aip_SUP_and_AICs where
  Aip_SUP_and_AICs x <> Aip_SUP_and_AICs y =
    Aip_SUP_and_AICs (x <> y)

instance Monoid Aip_SUP_and_AICs where
  mappend =
    (<>)
  mempty =
    Aip_SUP_and_AICs mempty

instance FromJSON Aip_SUP_and_AICs where
  parseJSON =
    withArray "Aip_SUP_and_AICs" $ \v ->
      Aip_SUP_and_AICs <$> traverse parseJSON (toList v)

instance ToJSON Aip_SUP_and_AICs where
  toJSON (Aip_SUP_and_AICs x) =
    toJSON x

instance Wrapped Aip_SUP_and_AICs where
  type Unwrapped Aip_SUP_and_AICs =
    [Aip_SUP_and_AIC]
  _Wrapped' =
    iso (\(Aip_SUP_and_AICs x) -> x) Aip_SUP_and_AICs

instance Aip_SUP_and_AICs ~ x =>
  Rewrapped Aip_SUP_and_AICs x

instance Cons Aip_SUP_and_AICs Aip_SUP_and_AICs Aip_SUP_and_AIC Aip_SUP_and_AIC where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc Aip_SUP_and_AICs Aip_SUP_and_AICs Aip_SUP_and_AIC Aip_SUP_and_AIC where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each Aip_SUP_and_AICs Aip_SUP_and_AICs Aip_SUP_and_AIC Aip_SUP_and_AIC where
  each =
    _Wrapped . each

instance Reversing Aip_SUP_and_AICs where
  reversing =
    _Wrapped %~ reversing

instance Plated Aip_SUP_and_AICs where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue Aip_SUP_and_AICs = Aip_SUP_and_AIC
type instance Index Aip_SUP_and_AICs = Int
instance Ixed Aip_SUP_and_AICs where
  ix i =
    _Wrapped . ix i

class AsAip_SUP_and_AICs a where
  _Aip_SUP_and_AICs ::
    Prism' a Aip_SUP_and_AICs
  default _Aip_SUP_and_AICs ::
    IsAip_SUP_and_AICs a =>
    Prism' a Aip_SUP_and_AICs
  _Aip_SUP_and_AICs =
    _IsAip_SUP_and_AICs
    
instance AsAip_SUP_and_AICs Aip_SUP_and_AICs where
  _Aip_SUP_and_AICs =
    id

class FoldAip_SUP_and_AICs a where
  _FoldAip_SUP_and_AICs ::
    Fold a Aip_SUP_and_AICs
    
instance FoldAip_SUP_and_AICs Aip_SUP_and_AICs where
  _FoldAip_SUP_and_AICs =
    id

class FoldAip_SUP_and_AICs a => GetAip_SUP_and_AICs a where
  _GetAip_SUP_and_AICs ::
    Getter a Aip_SUP_and_AICs
  default _GetAip_SUP_and_AICs ::
    HasAip_SUP_and_AICs a =>
    Getter a Aip_SUP_and_AICs
  _GetAip_SUP_and_AICs =
    aip_SUP_and_AICs
    
instance GetAip_SUP_and_AICs Aip_SUP_and_AICs where
  _GetAip_SUP_and_AICs =
    id

class SetAip_SUP_and_AICs a where
  _SetAip_SUP_and_AICs ::
    Setter' a Aip_SUP_and_AICs
  default _SetAip_SUP_and_AICs ::
    ManyAip_SUP_and_AICs a =>
    Setter' a Aip_SUP_and_AICs
  _SetAip_SUP_and_AICs =
    _ManyAip_SUP_and_AICs

instance SetAip_SUP_and_AICs Aip_SUP_and_AICs where
  _SetAip_SUP_and_AICs =
    id

class (FoldAip_SUP_and_AICs a, SetAip_SUP_and_AICs a) => ManyAip_SUP_and_AICs a where
  _ManyAip_SUP_and_AICs ::
    Traversal' a Aip_SUP_and_AICs

instance ManyAip_SUP_and_AICs Aip_SUP_and_AICs where
  _ManyAip_SUP_and_AICs =
    id

class (GetAip_SUP_and_AICs a, ManyAip_SUP_and_AICs a) => HasAip_SUP_and_AICs a where
  aip_SUP_and_AICs ::
    Lens' a Aip_SUP_and_AICs
  default aip_SUP_and_AICs ::
    IsAip_SUP_and_AICs a =>
    Lens' a Aip_SUP_and_AICs
  aip_SUP_and_AICs =
    _IsAip_SUP_and_AICs

instance HasAip_SUP_and_AICs Aip_SUP_and_AICs where
  aip_SUP_and_AICs =
    id

class (HasAip_SUP_and_AICs a, AsAip_SUP_and_AICs a) => IsAip_SUP_and_AICs a where
  _IsAip_SUP_and_AICs ::
    Iso' a Aip_SUP_and_AICs
    
instance IsAip_SUP_and_AICs Aip_SUP_and_AICs where
  _IsAip_SUP_and_AICs =
    id

instance SetAip_SUP_and_AICs () where
instance FoldAip_SUP_and_AICs () where
  _FoldAip_SUP_and_AICs =
    _ManyAip_SUP_and_AICs
instance ManyAip_SUP_and_AICs () where
  _ManyAip_SUP_and_AICs _ x =
    pure x

----

instance SetHref Aip_SUP_and_AICs where
instance FoldHref Aip_SUP_and_AICs where
  _FoldHref =
    _ManyHref

instance ManyHref Aip_SUP_and_AICs where
  _ManyHref =
    _Wrapped . traverse . _ManyHref

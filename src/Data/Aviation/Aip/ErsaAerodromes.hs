{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.ErsaAerodromes(
  ErsaAerodromes(..)
, AsErsaAerodromes(..)
, FoldErsaAerodromes(..)
, GetErsaAerodromes(..)
, SetErsaAerodromes(..)
, ManyErsaAerodromes(..)
, HasErsaAerodromes(..)
, IsErsaAerodromes(..)    
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.ErsaAerodrome(ErsaAerodrome)
import Data.Aviation.Aip.Href(SetHref, FoldHref, ManyHref(_ManyHref), FoldHref(_FoldHref))
import Papa

newtype ErsaAerodromes =
  ErsaAerodromes
    [ErsaAerodrome]
  deriving (Eq, Ord, Show)

instance Semigroup ErsaAerodromes where
  ErsaAerodromes x <> ErsaAerodromes y =
    ErsaAerodromes (x <> y)

instance Monoid ErsaAerodromes where
  mappend =
    (<>)
  mempty =
    ErsaAerodromes []

instance Wrapped ErsaAerodromes where
  type Unwrapped ErsaAerodromes =
    [ErsaAerodrome]
  _Wrapped' =
    iso (\(ErsaAerodromes x) -> x) ErsaAerodromes

instance ErsaAerodromes ~ x =>
  Rewrapped ErsaAerodromes x

instance FromJSON ErsaAerodromes where
  parseJSON =
    withArray "ErsaAerodromes" $ \v ->
      ErsaAerodromes <$> traverse parseJSON (toList v)

instance ToJSON ErsaAerodromes where
  toJSON (ErsaAerodromes x) =
    toJSON x

instance Cons ErsaAerodromes ErsaAerodromes ErsaAerodrome ErsaAerodrome where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc ErsaAerodromes ErsaAerodromes ErsaAerodrome ErsaAerodrome where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each ErsaAerodromes ErsaAerodromes ErsaAerodrome ErsaAerodrome where
  each =
    _Wrapped . each

instance Reversing ErsaAerodromes where
  reversing =
    _Wrapped %~ reversing

instance Plated ErsaAerodromes where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue ErsaAerodromes = ErsaAerodrome
type instance Index ErsaAerodromes = Int
instance Ixed ErsaAerodromes where
  ix i =
    _Wrapped . ix i

class ManyErsaAerodromes a => AsErsaAerodromes a where
  _ErsaAerodromes ::
    Prism' a ErsaAerodromes
  default _ErsaAerodromes ::
    IsErsaAerodromes a =>
    Prism' a ErsaAerodromes
  _ErsaAerodromes =
    _IsErsaAerodromes
    
instance AsErsaAerodromes ErsaAerodromes where
  _ErsaAerodromes =
    id

class FoldErsaAerodromes a where
  _FoldErsaAerodromes ::
    Fold a ErsaAerodromes
    
instance FoldErsaAerodromes ErsaAerodromes where
  _FoldErsaAerodromes =
    id

class FoldErsaAerodromes a => GetErsaAerodromes a where
  _GetErsaAerodromes ::
    Getter a ErsaAerodromes
  default _GetErsaAerodromes ::
    HasErsaAerodromes a =>
    Getter a ErsaAerodromes
  _GetErsaAerodromes =
    ersaAerodromes
    
instance GetErsaAerodromes ErsaAerodromes where
  _GetErsaAerodromes =
    id

class SetErsaAerodromes a where
  _SetErsaAerodromes ::
    Setter' a ErsaAerodromes
  default _SetErsaAerodromes ::
    ManyErsaAerodromes a =>
    Setter' a ErsaAerodromes
  _SetErsaAerodromes =
    _ManyErsaAerodromes

instance SetErsaAerodromes ErsaAerodromes where
  _SetErsaAerodromes =
    id

class (FoldErsaAerodromes a, SetErsaAerodromes a) => ManyErsaAerodromes a where
  _ManyErsaAerodromes ::
    Traversal' a ErsaAerodromes

instance ManyErsaAerodromes ErsaAerodromes where
  _ManyErsaAerodromes =
    id

class (GetErsaAerodromes a, ManyErsaAerodromes a) => HasErsaAerodromes a where
  ersaAerodromes ::
    Lens' a ErsaAerodromes
  default ersaAerodromes ::
    IsErsaAerodromes a =>
    Lens' a ErsaAerodromes
  ersaAerodromes =
    _IsErsaAerodromes

instance HasErsaAerodromes ErsaAerodromes where
  ersaAerodromes =
    id

class (HasErsaAerodromes a, AsErsaAerodromes a) => IsErsaAerodromes a where
  _IsErsaAerodromes ::
    Iso' a ErsaAerodromes
    
instance IsErsaAerodromes ErsaAerodromes where
  _IsErsaAerodromes =
    id

instance SetErsaAerodromes () where
instance FoldErsaAerodromes () where
  _FoldErsaAerodromes =
    _ManyErsaAerodromes
instance ManyErsaAerodromes () where
  _ManyErsaAerodromes _ x =
    pure x

----

instance SetHref ErsaAerodromes where
instance FoldHref ErsaAerodromes where
  _FoldHref =
    _ManyHref

instance ManyHref ErsaAerodromes where
  _ManyHref =
    _Wrapped . traverse . _ManyHref

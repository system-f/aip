{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.AipDate(
  AipDate(..)
, AsAipDate(..)
, FoldAipDate(..)
, GetAipDate(..)
, SetAipDate(..)
, ManyAipDate(..)
, HasAipDate(..)
, IsAipDate(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype AipDate =
  AipDate
    String
  deriving (Eq, Ord, Show)

instance FromJSON AipDate where
  parseJSON v =
    AipDate <$> parseJSON v

instance ToJSON AipDate where
  toJSON (AipDate x) =
    toJSON x

instance Semigroup AipDate where
  AipDate x <> AipDate y =
    AipDate (x <> y)

instance Monoid AipDate where
  mappend =
    (<>)
  mempty =
    AipDate mempty

instance Cons AipDate AipDate Char Char where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc AipDate AipDate Char Char where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each AipDate AipDate Char Char where
  each =
    _Wrapped . each

instance Reversing AipDate where
  reversing =
    _Wrapped %~ reversing

instance Plated AipDate where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue AipDate = Char
type instance Index AipDate = Int
instance Ixed AipDate where
  ix i =
    _Wrapped . ix i

instance Wrapped AipDate where
  type Unwrapped AipDate = String
  _Wrapped' =
    iso
      (\(AipDate x) -> x)
      AipDate

instance AipDate ~ a =>
  Rewrapped AipDate a

class ManyAipDate a => AsAipDate a where
  _AipDate ::
    Prism' a AipDate
  default _AipDate ::
    IsAipDate a =>
    Prism' a AipDate
  _AipDate =
    _IsAipDate

instance AsAipDate AipDate where
  _AipDate =
    id
  
instance AsAipDate String where
  _AipDate =
    from _Wrapped

class FoldAipDate a where
  _FoldAipDate ::
    Fold a AipDate
    
instance FoldAipDate AipDate where
  _FoldAipDate =
    id

instance FoldAipDate String where
  _FoldAipDate =
    from _Wrapped

class FoldAipDate a => GetAipDate a where
  _GetAipDate ::
    Getter a AipDate
  default _GetAipDate ::
    HasAipDate a =>
    Getter a AipDate
  _GetAipDate =
    aipDate
    
instance GetAipDate AipDate where
  _GetAipDate =
    id

instance GetAipDate String where
  _GetAipDate =
    from _Wrapped

class SetAipDate a where
  _SetAipDate ::
    Setter' a AipDate
  default _SetAipDate ::
    ManyAipDate a =>
    Setter' a AipDate
  _SetAipDate =
    _ManyAipDate
    
instance SetAipDate AipDate where
  _SetAipDate =
    id

instance SetAipDate String where
  _SetAipDate =
    from _Wrapped

class (FoldAipDate a, SetAipDate a) => ManyAipDate a where
  _ManyAipDate ::
    Traversal' a AipDate

instance ManyAipDate AipDate where
  _ManyAipDate =
    id

instance ManyAipDate String where
  _ManyAipDate =
    from _Wrapped

class (GetAipDate a, ManyAipDate a) => HasAipDate a where
  aipDate ::
    Lens' a AipDate
  default aipDate ::
    IsAipDate a =>
    Lens' a AipDate
  aipDate =
    _IsAipDate

instance HasAipDate AipDate where
  aipDate =
    id

instance HasAipDate String where
  aipDate =
    from _Wrapped

class (HasAipDate a, AsAipDate a) => IsAipDate a where
  _IsAipDate ::
    Iso' a AipDate
    
instance IsAipDate AipDate where
  _IsAipDate =
    id

instance IsAipDate String where
  _IsAipDate =
    from _Wrapped

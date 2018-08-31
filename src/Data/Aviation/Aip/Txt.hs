{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.Txt (
  Txt(..)
, AsTxt(..)
, FoldTxt(..)
, GetTxt(..)
, SetTxt(..)
, ManyTxt(..)
, HasTxt(..)
, IsTxt(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype Txt =
  Txt
    String
  deriving (Eq, Ord, Show)

instance FromJSON Txt where
  parseJSON v =
    Txt <$> parseJSON v

instance ToJSON Txt where
  toJSON (Txt x) =
    toJSON x

instance Semigroup Txt where
  Txt x <> Txt y =
    Txt (x <> y)

instance Monoid Txt where
  mappend =
    (<>)
  mempty =
    Txt mempty

instance Cons Txt Txt Char Char where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc Txt Txt Char Char where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each Txt Txt Char Char where
  each =
    _Wrapped . each

instance Reversing Txt where
  reversing =
    _Wrapped %~ reversing

instance Plated Txt where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue Txt = Char
type instance Index Txt = Int
instance Ixed Txt where
  ix i =
    _Wrapped . ix i

instance AsEmpty Txt where
  _Empty =
    _Wrapped . _Empty

instance Wrapped Txt where
  type Unwrapped Txt = String
  _Wrapped' =
    iso
      (\(Txt x) -> x)
      Txt

instance Txt ~ a =>
  Rewrapped Txt a

class AsTxt a where
  _Txt ::
    Prism' a Txt
  default _Txt ::
    IsTxt a =>
    Prism' a Txt
  _Txt =
    _IsTxt
    
instance AsTxt Txt where
  _Txt =
    id
  
instance AsTxt String where
  _Txt =
    from _Wrapped

class FoldTxt a where
  _FoldTxt ::
    Fold a Txt
    
instance FoldTxt Txt where
  _FoldTxt =
    id

instance FoldTxt String where
  _FoldTxt =
    from _Wrapped

class FoldTxt a => GetTxt a where
  _GetTxt ::
    Getter a Txt
  default _GetTxt ::
    HasTxt a =>
    Getter a Txt
  _GetTxt =
    txt
    
instance GetTxt Txt where
  _GetTxt =
    id

instance GetTxt String where
  _GetTxt =
    from _Wrapped

class SetTxt a where
  _SetTxt ::
    Setter' a Txt
  default _SetTxt ::
    ManyTxt a =>
    Traversal' a Txt
  _SetTxt =
    _ManyTxt

instance SetTxt Txt where
  _SetTxt =
    id

instance SetTxt String where
  _SetTxt =
    from _Wrapped

class (FoldTxt a, SetTxt a) => ManyTxt a where
  _ManyTxt ::
    Traversal' a Txt

instance ManyTxt Txt where
  _ManyTxt =
    id

instance ManyTxt String where
  _ManyTxt =
    from _Wrapped

class (GetTxt a, ManyTxt a) => HasTxt a where
  txt ::
    Lens' a Txt
  default txt ::
    IsTxt a =>
    Lens' a Txt
  txt =
    _IsTxt

instance HasTxt Txt where
  txt =
    id

instance HasTxt String where
  txt =
    from _Wrapped

class (HasTxt a, AsTxt a) => IsTxt a where
  _IsTxt ::
    Iso' a Txt
    
instance IsTxt Txt where
  _IsTxt =
    id

instance IsTxt String where
  _IsTxt =
    from _Wrapped

instance SetTxt () where
instance FoldTxt () where
  _FoldTxt =
    _ManyTxt
instance ManyTxt () where
  _ManyTxt _ x =
    pure x

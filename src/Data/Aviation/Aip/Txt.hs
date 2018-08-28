{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Txt(
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
    
instance GetTxt Txt where
  _GetTxt =
    id

instance GetTxt String where
  _GetTxt =
    from _Wrapped

class SetTxt a where
  _SetTxt ::
    Setter' a Txt
    
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

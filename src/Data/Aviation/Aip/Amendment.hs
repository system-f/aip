{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.Amendment(
  Amendment(..)
, AsAmendment(..)
, FoldAmendment(..)
, GetAmendment(..)
, SetAmendment(..)
, ManyAmendment(..)
, HasAmendment(..)
, IsAmendment(..)
) where

import Control.Category((.), id)
import Control.Applicative(pure)
import Control.Lens
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Int(Int)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Prelude(Show)

newtype Amendment =
  Amendment
    String
  deriving (Eq, Ord, Show)

instance FromJSON Amendment where
  parseJSON v =
    Amendment <$> parseJSON v

instance ToJSON Amendment where
  toJSON (Amendment x) =
    toJSON x

instance Semigroup Amendment where
  Amendment x <> Amendment y =
    Amendment (x <> y)

instance Monoid Amendment where
  mappend =
    (<>)
  mempty =
    Amendment mempty

instance Cons Amendment Amendment Char Char where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc Amendment Amendment Char Char where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each Amendment Amendment Char Char where
  each =
    _Wrapped . each

instance Reversing Amendment where
  reversing =
    _Wrapped %~ reversing

instance Plated Amendment where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue Amendment = Char
type instance Index Amendment = Int
instance Ixed Amendment where
  ix i =
    _Wrapped . ix i

instance Wrapped Amendment where
  type Unwrapped Amendment = String
  _Wrapped' =
    iso
      (\(Amendment x) -> x)
      Amendment

instance Amendment ~ a =>
  Rewrapped Amendment a

class ManyAmendment a => AsAmendment a where
  _Amendment ::
    Prism' a Amendment
  default _Amendment ::
    IsAmendment a =>
    Prism' a Amendment
  _Amendment =
    _IsAmendment
    
instance AsAmendment Amendment where
  _Amendment =
    id
  
instance AsAmendment String where
  _Amendment =
    from _Wrapped

class FoldAmendment a where
  _FoldAmendment ::
    Fold a Amendment
    
instance FoldAmendment Amendment where
  _FoldAmendment =
    id

instance FoldAmendment String where
  _FoldAmendment =
    from _Wrapped

class FoldAmendment a => GetAmendment a where
  _GetAmendment ::
    Getter a Amendment
  default _GetAmendment ::
    HasAmendment a =>
    Getter a Amendment
  _GetAmendment =
    amendment
    
instance GetAmendment Amendment where
  _GetAmendment =
    id

instance GetAmendment String where
  _GetAmendment =
    from _Wrapped

class SetAmendment a where
  _SetAmendment ::
    Setter' a Amendment
  default _SetAmendment ::
    ManyAmendment a =>
    Setter' a Amendment
  _SetAmendment =
    _ManyAmendment

instance SetAmendment Amendment where
  _SetAmendment =
    id

instance SetAmendment String where
  _SetAmendment =
    from _Wrapped

class (FoldAmendment a, SetAmendment a) => ManyAmendment a where
  _ManyAmendment ::
    Traversal' a Amendment

instance ManyAmendment Amendment where
  _ManyAmendment =
    id

instance ManyAmendment String where
  _ManyAmendment =
    from _Wrapped

class (GetAmendment a, ManyAmendment a) => HasAmendment a where
  amendment ::
    Lens' a Amendment
  default amendment ::
    IsAmendment a =>
    Lens' a Amendment
  amendment =
    _IsAmendment

instance HasAmendment Amendment where
  amendment =
    id

instance HasAmendment String where
  amendment =
    from _Wrapped

class (HasAmendment a, AsAmendment a) => IsAmendment a where
  _IsAmendment ::
    Iso' a Amendment
    
instance IsAmendment Amendment where
  _IsAmendment =
    id

instance IsAmendment String where
  _IsAmendment =
    from _Wrapped

instance SetAmendment () where
instance FoldAmendment () where
  _FoldAmendment =
    _ManyAmendment
instance ManyAmendment () where
  _ManyAmendment _ x =
    pure x

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

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

instance Wrapped Amendment where
  type Unwrapped Amendment = String
  _Wrapped' =
    iso
      (\(Amendment x) -> x)
      Amendment

instance Amendment ~ a =>
  Rewrapped Amendment a

class AsAmendment a where
  _Amendment ::
    Prism' a Amendment
    
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
    
instance GetAmendment Amendment where
  _GetAmendment =
    id

instance GetAmendment String where
  _GetAmendment =
    from _Wrapped

class SetAmendment a where
  _SetAmendment ::
    Setter' a Amendment
    
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

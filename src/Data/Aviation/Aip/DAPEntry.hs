{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.DAPEntry(
  DAPEntry(..)
, AsDAPEntry(..)
, FoldDAPEntry(..)
, GetDAPEntry(..)
, SetDAPEntry(..)
, ManyDAPEntry(..)
, HasDAPEntry(..)
, IsDAPEntry(..)    
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.AipDate(AipDate)
import Data.Aviation.Aip.Amendment(Amendment)
import Data.Aviation.Aip.Href(Href)
import Data.Aviation.Aip.Txt(Txt)
import Papa hiding ((.=))

data DAPEntry =
  DAPEntry
    Href
    Txt
    AipDate
    Amendment
  deriving (Eq, Ord, Show)

instance FromJSON DAPEntry where
  parseJSON =
    withObject "DAPEntry" $ \v ->
      DAPEntry <$>
        v .: "href" <*>
        v .: "txt" <*>
        v .: "date" <*>
        v .: "amendment"

instance ToJSON DAPEntry where
  toJSON (DAPEntry href txt date amendment) =
    object ["href" .= href, "txt" .= txt, "date" .= date, "amendment" .= amendment]

class AsDAPEntry a where
  _DAPEntry ::
    Prism' a DAPEntry
  default _DAPEntry ::
    IsDAPEntry a =>
    Prism' a DAPEntry
  _DAPEntry =
    _IsDAPEntry
    
instance AsDAPEntry DAPEntry where
  _DAPEntry =
    id

class FoldDAPEntry a where
  _FoldDAPEntry ::
    Fold a DAPEntry
    
instance FoldDAPEntry DAPEntry where
  _FoldDAPEntry =
    id

class FoldDAPEntry a => GetDAPEntry a where
  _GetDAPEntry ::
    Getter a DAPEntry
  default _GetDAPEntry ::
    HasDAPEntry a =>
    Getter a DAPEntry
  _GetDAPEntry =
    dapEntry
    
instance GetDAPEntry DAPEntry where
  _GetDAPEntry =
    id

class SetDAPEntry a where
  _SetDAPEntry ::
    Setter' a DAPEntry
  default _SetDAPEntry ::
    ManyDAPEntry a =>
    Setter' a DAPEntry
  _SetDAPEntry =
    _ManyDAPEntry

instance SetDAPEntry DAPEntry where
  _SetDAPEntry =
    id

class (FoldDAPEntry a, SetDAPEntry a) => ManyDAPEntry a where
  _ManyDAPEntry ::
    Traversal' a DAPEntry

instance ManyDAPEntry DAPEntry where
  _ManyDAPEntry =
    id

class (GetDAPEntry a, ManyDAPEntry a) => HasDAPEntry a where
  dapEntry ::
    Lens' a DAPEntry
  default dapEntry ::
    IsDAPEntry a =>
    Lens' a DAPEntry
  dapEntry =
    _IsDAPEntry

instance HasDAPEntry DAPEntry where
  dapEntry =
    id

class (HasDAPEntry a, AsDAPEntry a) => IsDAPEntry a where
  _IsDAPEntry ::
    Iso' a DAPEntry
    
instance IsDAPEntry DAPEntry where
  _IsDAPEntry =
    id

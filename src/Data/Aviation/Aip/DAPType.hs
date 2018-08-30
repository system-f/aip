{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.DAPType(
  DAPType(..)
, DAPType'
, AsDAPType(..)
, FoldDAPType(..)
, GetDAPType(..)
, SetDAPType(..)
, ManyDAPType(..)
, HasDAPType(..)
, IsDAPType(..)    
) where

import Control.Monad(fail)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), Value(Object), object, (.=))
import qualified Data.HashMap.Strict as HashMap(toList)
import Papa hiding ((.=))

data DAPType aerodrome =
  SpecNotManTOCDAP
  | ChecklistTOCDAP
  | LegendInfoTablesTOCDAP
  | AeroProcChartsTOCDAP aerodrome
  deriving (Eq, Ord, Show)

type DAPType' =
  DAPType ()

instance FromJSON aerodrome => FromJSON (DAPType aerodrome) where
  parseJSON (Object z) =
    case HashMap.toList z of
      [("SpecNotManTOCDAP", q)] ->
        (\() -> SpecNotManTOCDAP) <$> parseJSON q   
      [("ChecklistTOCDAP", q)] ->
        (\() -> ChecklistTOCDAP) <$> parseJSON q
      [("LegendInfoTablesTOCDAP", q)] ->
        (\() -> LegendInfoTablesTOCDAP) <$> parseJSON q
      [("AeroProcChartsTOCDAP", q)] ->
        AeroProcChartsTOCDAP <$> parseJSON q
      _ ->
        fail "DAPType"
  parseJSON _ =
    fail "DAPType"

instance ToJSON aerodrome => ToJSON (DAPType aerodrome) where
  toJSON SpecNotManTOCDAP =
    object ["SpecNotManTOCDAP" .= toJSON ()]
  toJSON ChecklistTOCDAP =
    object ["ChecklistTOCDAP" .= toJSON ()]
  toJSON LegendInfoTablesTOCDAP =
    object ["LegendInfoTablesTOCDAP" .= toJSON ()]
  toJSON (AeroProcChartsTOCDAP x) =
    object ["AeroProcChartsTOCDAP" .= toJSON x]

class AsDAPType a where
  _DAPType ::
    Prism (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
  default _DAPType ::
    IsDAPType a =>
    Prism (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
  _DAPType =
    _IsDAPType
    
instance AsDAPType DAPType where
  _DAPType =
    id

class FoldDAPType a where
  _FoldDAPType ::
    Fold (a aerodrome) (DAPType aerodrome)
    
instance FoldDAPType DAPType where
  _FoldDAPType =
    id

class FoldDAPType a => GetDAPType a where
  _GetDAPType ::
    Getter (a aerodrome) (DAPType aerodrome)
  default _GetDAPType ::
    HasDAPType a =>
    Getter (a aerodrome) (DAPType aerodrome)
  _GetDAPType =
    dapType

instance GetDAPType DAPType where
  _GetDAPType =
    id

class SetDAPType a where
  _SetDAPType ::
    Setter (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
  default _SetDAPType ::
    ManyDAPType a =>
    Setter (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
  _SetDAPType =
    _ManyDAPType

instance SetDAPType DAPType where
  _SetDAPType =
    id

class (FoldDAPType a, SetDAPType a) => ManyDAPType a where
  _ManyDAPType ::
    Traversal (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')

instance ManyDAPType DAPType where
  _ManyDAPType =
    id

class (GetDAPType a, ManyDAPType a) => HasDAPType a where
  dapType ::
    Lens (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
  default dapType ::
    IsDAPType a =>
    Lens (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
  dapType =
    _IsDAPType

instance HasDAPType DAPType where
  dapType =
    id

class (HasDAPType a, AsDAPType a) => IsDAPType a where
  _IsDAPType ::
    Iso (a aerodrome) (a aerodrome') (DAPType aerodrome) (DAPType aerodrome')
    
instance IsDAPType DAPType where
  _IsDAPType =
    id

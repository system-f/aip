{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.DAPType(
  DAPType(..)
, DAPType'
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

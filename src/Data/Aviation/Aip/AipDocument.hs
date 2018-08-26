{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.AipDocument(
  AipDocument(..)
, AipDocument1
, AipDocument2
) where

import Control.Monad(fail)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), Value(Object), object, (.=))
import Data.Aviation.Aip.DAPDocs(DAPDocs)
import Data.Aviation.Aip.Ersa(Ersa)
import Data.Aviation.Aip.Aip_SUP_and_AICs(Aip_SUP_and_AICs)
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ListItemLinks1(ListItemLinks1)
import qualified Data.HashMap.Strict as HashMap(toList)
import Papa hiding ((.=))

data AipDocument book charts sup_aic dap ersa =
  Aip_Book String String book
  | Aip_Charts String String charts
  | Aip_SUP_AIC String sup_aic
  | Aip_Summary_SUP_AIC String String
  | Aip_DAP String String dap
  | Aip_DAH String String
  | Aip_ERSA String String ersa
  | Aip_AandB_Charts String
  deriving (Eq, Ord, Show)

type AipDocument1 =
  AipDocument () () () () ()

type AipDocument2 =
  AipDocument ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa

instance (FromJSON book, FromJSON charts, FromJSON sup_aic, FromJSON dap, FromJSON ersa) => FromJSON (AipDocument book charts sup_aic dap ersa) where
  parseJSON (Object z) =
    case HashMap.toList z of
      [("Aip_Book", q)] ->
        (\(u, t, x) -> Aip_Book u t x) <$> parseJSON q
      [("Aip_Charts", q)] ->
        (\(u, t, x) -> Aip_Charts u t x) <$> parseJSON q
      [("Aip_SUP_AIC", q)] ->
        (\(u, x) -> Aip_SUP_AIC u x) <$> parseJSON q
      [("Aip_Summary_SUP_AIC", q)] ->
        (\(u, x) -> Aip_Summary_SUP_AIC u x) <$> parseJSON q
      [("Aip_DAP", q)] ->
        (\(u, t, x) -> Aip_DAP u t x) <$> parseJSON q
      [("Aip_DAH", q)] ->
        (\(u, x) -> Aip_DAH u x) <$> parseJSON q
      [("Aip_ERSA", q)] ->
        (\(u, t, x) -> Aip_ERSA u t x) <$> parseJSON q
      [("Aip_AandB_Charts", q)] ->
        Aip_AandB_Charts <$> parseJSON q
      _ ->
        fail "AipDocument"
  parseJSON _ =
    fail "AipDocument"
    
instance (ToJSON book, ToJSON charts, ToJSON sup_aic, ToJSON dap, ToJSON ersa) => ToJSON (AipDocument book charts sup_aic dap ersa) where
  toJSON (Aip_Book u t x) =
    object ["Aip_Book" .= toJSON (u, t, x)]
  toJSON (Aip_Charts u t x) =
    object ["Aip_Charts" .= toJSON (u, t, x)]
  toJSON (Aip_SUP_AIC u x) =
    object ["Aip_SUP_AIC" .= toJSON (u, x)]
  toJSON (Aip_Summary_SUP_AIC u x) =
    object ["Aip_Summary_SUP_AIC" .= toJSON (u, x)]
  toJSON (Aip_DAP u t x) =
    object ["Aip_DAP" .= toJSON (u, t, x)]
  toJSON (Aip_DAH u x) =
    object ["Aip_DAH" .= toJSON (u, x)]
  toJSON (Aip_ERSA u t x) =
    object ["Aip_ERSA" .= toJSON (u, t, x)]
  toJSON (Aip_AandB_Charts q) =
    object ["Aip_AandB_Charts" .= toJSON q]
  
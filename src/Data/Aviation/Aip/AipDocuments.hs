{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.AipDocuments(
  AipDocuments(..)
, AipDocuments1
, AipDocuments2
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.Aip_SUP_and_AICs(Aip_SUP_and_AICs)
import Data.Aviation.Aip.AipDocument(AipDocument)
import Data.Aviation.Aip.DAPDocs(DAPDocs)
import Data.Aviation.Aip.Ersa(Ersa)
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ListItemLinks1(ListItemLinks1)
import Papa

newtype AipDocuments book charts sup_aic dap ersa =
  AipDocuments
    [AipDocument book charts sup_aic dap ersa]
  deriving (Eq, Ord, Show)

instance Monoid (AipDocuments book charts sup_aic dap ersa) where
  mempty =
    AipDocuments
      mempty
  AipDocuments x `mappend` AipDocuments y =
    AipDocuments (x `mappend` y)

type AipDocuments1 =
  AipDocuments () () () () ()

type AipDocuments2 =
  AipDocuments ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa

instance (FromJSON book, FromJSON charts, FromJSON sup_aic, FromJSON dap, FromJSON ersa) => FromJSON (AipDocuments book charts sup_aic dap ersa) where
  parseJSON =
    withArray "AipDocuments" $ \v ->
      AipDocuments <$> traverse parseJSON (toList v)

instance (ToJSON book, ToJSON charts, ToJSON sup_aic, ToJSON dap, ToJSON ersa) => ToJSON (AipDocuments book charts sup_aic dap ersa) where
  toJSON (AipDocuments x) =
    toJSON x

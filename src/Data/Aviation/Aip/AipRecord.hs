{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.AipRecord(
  AipRecord(..)
, ManyAipRecord(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.Aip_SUP_and_AICs(Aip_SUP_and_AICs)
import Data.Aviation.Aip.AipDocuments(AipDocuments)
import Data.Aviation.Aip.DAPDocs(DAPDocs)
import Data.Aviation.Aip.Ersa(Ersa)
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ListItemLinks1(ListItemLinks1)
import Data.Time(UTCTime)
import Papa hiding ((.=))

data AipRecord =
  AipRecord
    UTCTime
    (AipDocuments ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa)
  deriving (Eq, Show)

instance FromJSON AipRecord where
  parseJSON =
    withObject "AipRecord" $ \v ->
      AipRecord <$>
        v .: "utc" <*>
        v .: "documents"

instance ToJSON AipRecord where
  toJSON (AipRecord t p) =
    object ["utc" .= t, "documents" .= p]

class ManyAipRecord a where
  _ManyAipRecord ::
    Traversal' a AipRecord

instance ManyAipRecord AipRecord where
  _ManyAipRecord =
    id

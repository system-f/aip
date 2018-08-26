{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.DAPDoc(
  DAPDoc(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.DAPType(DAPType)
import Data.Aviation.Aip.DAPEntries(DAPEntries)
import Papa hiding ((.=))

data DAPDoc =
  DAPDoc
    (DAPType String)
    String -- url
    DAPEntries
  deriving (Eq, Ord, Show)

instance FromJSON DAPDoc where
  parseJSON =
    withObject "DAPDoc" $ \v ->
      DAPDoc <$>
        v .: "type" <*>
        v .: "href" <*>
        v .: "entries"

instance ToJSON DAPDoc where
  toJSON (DAPDoc typ href entries) =
    object ["type" .= typ, "href" .= href, "entries" .= entries]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.DAPEntry(
  DAPEntry(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Papa hiding ((.=))

data DAPEntry =
  DAPEntry
    String -- href
    String -- text
    String -- date
    String -- amend
  deriving (Eq, Ord, Show)

instance FromJSON DAPEntry where
  parseJSON =
    withObject "DAPEntry" $ \v ->
      DAPEntry <$>
        v .: "href" <*>
        v .: "text" <*>
        v .: "date" <*>
        v .: "amendment"

instance ToJSON DAPEntry where
  toJSON (DAPEntry href text date amendment) =
    object ["href" .= href, "text" .= text, "date" .= date, "amendment" .= amendment]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.DAPEntry(
  DAPEntry(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.Href(Href)
import Data.Aviation.Aip.Txt(Txt)
import Papa hiding ((.=))

data DAPEntry =
  DAPEntry
    Href
    Txt
    String -- date
    String -- amend
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

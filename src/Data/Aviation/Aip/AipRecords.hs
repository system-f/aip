{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.AipRecords(
  AipRecords(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.AipRecord(AipRecord, ManyAipRecord(_ManyAipRecord))
import Data.Aviation.Aip.SHA1(SHA1)
import Papa hiding ((.=))

data AipRecords =
  AipRecords
    SHA1
    (NonEmpty AipRecord)
  deriving (Eq, Show)

instance FromJSON AipRecords where
  parseJSON =
    withObject "AipRecords" $ \v ->
      AipRecords <$>
        v .: "sha1" <*>
        v .: "aiprecords"

instance ToJSON AipRecords where
  toJSON (AipRecords s r) =
    object ["sha1" .= s, "aiprecords" .= r]

instance ManyAipRecord AipRecords where
  _ManyAipRecord f (AipRecords s r) =
    AipRecords s <$> traverse f r

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Href(
  Href(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype Href =
  Href
    String
  deriving (Eq, Ord, Show)

instance FromJSON Href where
  parseJSON v =
    Href <$> parseJSON v

instance ToJSON Href where
  toJSON (Href x) =
    toJSON x

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.ErsaAerodrome(
  ErsaAerodrome(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Papa hiding ((.=))

data ErsaAerodrome =
  ErsaAerodrome
    String
    String
    (Maybe String)
  deriving (Eq, Ord, Show)

instance FromJSON ErsaAerodrome where
  parseJSON =
    withObject "ErsaAerodrome" $ \v ->
      ErsaAerodrome <$>
        v .: "aerodrome" <*>
        v .: "fac_href" <*>
        v .: "rds_href"

instance ToJSON ErsaAerodrome where
  toJSON (ErsaAerodrome aerodrome fac rds) =
    object ["aerodrome" .= aerodrome, "fac_href" .= fac, "rds_href" .= rds]

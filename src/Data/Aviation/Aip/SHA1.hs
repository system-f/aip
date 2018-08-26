{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.SHA1(
  SHA1(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Digest.SHA1(Word160(Word160))
import Papa

newtype SHA1 =
  SHA1
    Word160
  deriving (Eq, Show)

instance FromJSON SHA1 where
  parseJSON v =
    (\(b0, b1, b2, b3, b4) -> SHA1 (Word160 b0 b1 b2 b3 b4)) <$> parseJSON v

instance ToJSON SHA1 where
  toJSON (SHA1 (Word160 b0 b1 b2 b3 b4)) =
    toJSON (b0, b1, b2, b3, b4)

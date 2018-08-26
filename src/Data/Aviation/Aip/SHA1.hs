{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.SHA1(
  SHA1(..)
, hash
, hashHex
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Digest.SHA1(Word160(Word160))
import qualified Data.Digest.SHA1 as SHA1(hash, toInteger)
import Data.Word(Word8)
import Numeric(showHex)
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

hash ::
  [Word8]
  -> SHA1
hash =
  SHA1 . SHA1.hash

hashHex ::
  SHA1
  -> ShowS
hashHex (SHA1 x) =
  showHex (SHA1.toInteger x)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.SHA1(
  SHA1(..)
, AsSHA1(..)
, FoldSHA1(..)
, GetSHA1(..)
, SetSHA1(..)
, ManySHA1(..)
, HasSHA1(..)
, IsSHA1(..)      
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

class AsSHA1 a where
  _SHA1 ::
    Prism' a SHA1
  default _SHA1 ::
    IsSHA1 a =>
    Prism' a SHA1
  _SHA1 =
    _IsSHA1
    
instance AsSHA1 SHA1 where
  _SHA1 =
    id

class FoldSHA1 a where
  _FoldSHA1 ::
    Fold a SHA1
    
instance FoldSHA1 SHA1 where
  _FoldSHA1 =
    id

class FoldSHA1 a => GetSHA1 a where
  _GetSHA1 ::
    Getter a SHA1
  default _GetSHA1 ::
    HasSHA1 a =>
    Getter a SHA1
  _GetSHA1 =
    sha1
    
instance GetSHA1 SHA1 where
  _GetSHA1 =
    id

class SetSHA1 a where
  _SetSHA1 ::
    Setter' a SHA1
  default _SetSHA1 ::
    ManySHA1 a =>
    Setter' a SHA1
  _SetSHA1 =
    _ManySHA1

instance SetSHA1 SHA1 where
  _SetSHA1 =
    id

class (FoldSHA1 a, SetSHA1 a) => ManySHA1 a where
  _ManySHA1 ::
    Traversal' a SHA1

instance ManySHA1 SHA1 where
  _ManySHA1 =
    id

class (GetSHA1 a, ManySHA1 a) => HasSHA1 a where
  sha1 ::
    Lens' a SHA1
  default sha1 ::
    IsSHA1 a =>
    Lens' a SHA1
  sha1 =
    _IsSHA1

instance HasSHA1 SHA1 where
  sha1 =
    id

class (HasSHA1 a, AsSHA1 a) => IsSHA1 a where
  _IsSHA1 ::
    Iso' a SHA1
    
instance IsSHA1 SHA1 where
  _IsSHA1 =
    id

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

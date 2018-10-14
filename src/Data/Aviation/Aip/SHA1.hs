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
, showsHash
, strHash
) where

import Control.Category((.), id)
import Control.Applicative(pure)
import Control.Lens
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Digest.SHA1(Word160(Word160))
import qualified Data.Digest.SHA1 as SHA1(hash, toInteger)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Maybe(Maybe(Nothing))
import Data.String(String)
import Data.Word(Word8, Word32)
import Numeric(showHex, readHex)
import Prelude(Show, ShowS, ReadS)

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

class ManySHA1 a => AsSHA1 a where
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

instance SetSHA1 () where
instance FoldSHA1 () where
  _FoldSHA1 =
    _ManySHA1
instance ManySHA1 () where
  _ManySHA1 _ x =
    pure x

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

showsHash ::
  HasSHA1 s =>
  s
  -> ShowS
showsHash x =
  hashHex (x ^. sha1)

strHash ::
  Prism'
    String
    SHA1
strHash =
  prism'
    (\x -> showsHash x "")
    (\h ->  case h of
              (a0:a1:a2:a3:a4:a5:a6:a7:b0:b1:b2:b3:b4:b5:b6:b7:c0:c1:c2:c3:c4:c5:c6:c7:d0:d1:d2:d3:d4:d5:d6:d7:e0:e1:e2:e3:e4:e5:e6:e7:[]) ->
                let word32 w = 
                      (readHex :: ReadS Word32) w ^? _head . _1
                in  do  w1 <- word32 [a0,a1,a2,a3,a4,a5,a6,a7]
                        w2 <- word32 [b0,b1,b2,b3,b4,b5,b6,b7]
                        w3 <- word32 [c0,c1,c2,c3,c4,c5,c6,c7]
                        w4 <- word32 [d0,d1,d2,d3,d4,d5,d6,d7]
                        w5 <- word32 [e0,e1,e2,e3,e4,e5,e6,e7]
                        pure (SHA1 (Word160 w1 w2 w3 w4 w5))
              _ ->
                Nothing)
    
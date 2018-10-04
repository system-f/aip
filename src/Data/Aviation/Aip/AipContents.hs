{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.AipContents(
  AipContents(..)
, AsAipContents(..)
, FoldAipContents(..)
, GetAipContents(..)
, SetAipContents(..)
, ManyAipContents(..)
, HasAipContents(..)
, IsAipContents(..)      
) where

import Control.Category((.), id)
import Control.Lens
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data AipContents =
  AipContents
    String -- path
    String -- query
    String -- body
  deriving (Eq, Ord, Show)

class ManyAipContents a => AsAipContents a where
  _AipContents ::
    Prism' a AipContents
  default _AipContents ::
    IsAipContents a =>
    Prism' a AipContents
  _AipContents =
    _IsAipContents
    
instance AsAipContents AipContents where
  _AipContents =
    id

class FoldAipContents a where
  _FoldAipContents ::
    Fold a AipContents
    
instance FoldAipContents AipContents where
  _FoldAipContents =
    id

class FoldAipContents a => GetAipContents a where
  _GetAipContents ::
    Getter a AipContents
  default _GetAipContents ::
    HasAipContents a =>
    Getter a AipContents
  _GetAipContents =
    aipContents
    
instance GetAipContents AipContents where
  _GetAipContents =
    id

class SetAipContents a where
  _SetAipContents ::
    Setter' a AipContents
  default _SetAipContents ::
    ManyAipContents a =>
    Setter' a AipContents
  _SetAipContents =
    _ManyAipContents

instance SetAipContents AipContents where
  _SetAipContents =
    id

class (FoldAipContents a, SetAipContents a) => ManyAipContents a where
  _ManyAipContents ::
    Traversal' a AipContents

instance ManyAipContents AipContents where
  _ManyAipContents =
    id

class (GetAipContents a, ManyAipContents a) => HasAipContents a where
  aipContents ::
    Lens' a AipContents
  default aipContents ::
    IsAipContents a =>
    Lens' a AipContents
  aipContents =
    _IsAipContents
  aipContentsPath ::
    Lens' a String
  aipContentsPath =
    aipContents . aipContentsPath
  aipContentsQuery ::
    Lens' a String
  aipContentsQuery =
    aipContents . aipContentsQuery
  aipContentsBody ::
    Lens' a String
  aipContentsBody =
    aipContents . aipContentsBody

instance HasAipContents AipContents where
  aipContents =
    id
  aipContentsPath f (AipContents p q b) =
    (\p' -> AipContents p' q b) <$> f p
  aipContentsQuery f (AipContents p q b) =
    (\q' -> AipContents p q' b) <$> f q
  aipContentsBody f (AipContents p q b) =
    (\b' -> AipContents p q b') <$> f b

class (HasAipContents a, AsAipContents a) => IsAipContents a where
  _IsAipContents ::
    Iso' a AipContents
    
instance IsAipContents AipContents where
  _IsAipContents =
    id

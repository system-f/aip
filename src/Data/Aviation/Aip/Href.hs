{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}

module Data.Aviation.Aip.Href(
  Href(..)
, AsHref(..)
, FoldHref(..)
, GetHref(..)
, SetHref(..)
, ManyHref(..)
, HasHref(..)
, IsHref(..)
, dropHrefFile
, aipPrefix
, windows_replace
) where

import Control.Category((.), id)
import Control.Applicative(pure, (<*>))
import Control.Lens
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Bool(bool)
import Data.Char(Char)
import Data.Eq(Eq((/=)))
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Data.Foldable(elem)
#endif
import Data.Functor((<$>))
import Data.Int(Int)
import Data.List(reverse, dropWhile, isPrefixOf)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Prelude(Show)

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

instance Semigroup Href where
  Href x <> Href y =
    Href (x <> y)

instance Monoid Href where
  mappend =
    (<>)
  mempty =
    Href mempty

instance Cons Href Href Char Char where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc Href Href Char Char where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each Href Href Char Char where
  each =
    _Wrapped . each

instance Reversing Href where
  reversing =
    _Wrapped %~ reversing

instance Plated Href where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue Href = Char
type instance Index Href = Int
instance Ixed Href where
  ix i =
    _Wrapped . ix i

instance AsEmpty Href where
  _Empty =
    _Wrapped . _Empty

instance Wrapped Href where
  type Unwrapped Href = String
  _Wrapped' =
    iso
      (\(Href x) -> x)
      Href

instance Href ~ a =>
  Rewrapped Href a

class ManyHref a => AsHref a where
  _Href ::
    Prism' a Href
  default _Href ::
    IsHref a =>
    Prism' a Href
  _Href =
    _IsHref
    
instance AsHref Href where
  _Href =
    id
  
instance AsHref String where
  _Href =
    from _Wrapped

class FoldHref a where
  _FoldHref ::
    Fold a Href
    
instance FoldHref Href where
  _FoldHref =
    id

instance FoldHref String where
  _FoldHref =
    from _Wrapped

class FoldHref a => GetHref a where
  _GetHref ::
    Getter a Href
  default _GetHref ::
    HasHref a =>
    Getter a Href
  _GetHref =
    href
    
instance GetHref Href where
  _GetHref =
    id

instance GetHref String where
  _GetHref =
    from _Wrapped

class SetHref a where
  _SetHref ::
    Setter' a Href
  default _SetHref ::
    ManyHref a =>
    Traversal' a Href
  _SetHref =
    _ManyHref

instance SetHref Href where
  _SetHref =
    id

instance SetHref String where
  _SetHref =
    from _Wrapped

class (FoldHref a, SetHref a) => ManyHref a where
  _ManyHref ::
    Traversal' a Href

instance ManyHref Href where
  _ManyHref =
    id

instance ManyHref String where
  _ManyHref =
    from _Wrapped

class (GetHref a, ManyHref a) => HasHref a where
  href ::
    Lens' a Href
  default href ::
    IsHref a =>
    Lens' a Href
  href =
    _IsHref

instance HasHref Href where
  href =
    id

instance HasHref String where
  href =
    from _Wrapped

class (HasHref a, AsHref a) => IsHref a where
  _IsHref ::
    Iso' a Href
    
instance IsHref Href where
  _IsHref =
    id

instance IsHref String where
  _IsHref =
    from _Wrapped

instance SetHref () where
instance FoldHref () where
  _FoldHref =
    _ManyHref
instance ManyHref () where
  _ManyHref _ x =
    pure x

dropHrefFile ::
  Href
  -> Href
dropHrefFile =
  (_Wrapped %~ reverse . dropWhile (/= '/') . reverse)

aipPrefix ::
  ManyHref s =>
  s
  -> s
aipPrefix =
  _ManyHref . _Wrapped %~ let p = "/aip/" in bool <$> (p <>) <*> id <*> isPrefixOf p

windows_replace ::
  String
  -> String
windows_replace x = 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  let win = "/\\:*\"?<>|"
      repl ch = bool ch '_' (ch `elem` win)
  in  repl <$> x
#else
  x
#endif                    

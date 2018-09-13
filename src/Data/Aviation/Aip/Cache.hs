{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.Cache(
  Cache(..)
, AsCache(..)
, FoldCache(..)
, GetCache(..)
, SetCache(..)
, ManyCache(..)
, HasCache(..)
, IsCache(..)
, AsReadCache(..)
, AsReadWriteCache(..)
, AsNoCache(..)
, isReadOrWriteCache
, isWriteCache
) where

import Control.Category(id)
import Control.Applicative(pure)
import Control.Lens
import Data.Bool(Bool, not)
import Data.Eq(Eq)
import Data.Foldable(any)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Prelude(Show)

data Cache =
  ReadCache
  | ReadWriteCache
  | NoCache
  deriving (Eq, Ord, Show)

class ManyCache a => AsCache a where
  _Cache ::
    Prism' a Cache
  default _Cache ::
    IsCache a =>
    Prism' a Cache
  _Cache =
    _IsCache
    
instance AsCache Cache where
  _Cache =
    id

class FoldCache a where
  _FoldCache ::
    Fold a Cache
    
instance FoldCache Cache where
  _FoldCache =
    id

class FoldCache a => GetCache a where
  _GetCache ::
    Getter a Cache
  default _GetCache ::
    HasCache a =>
    Getter a Cache
  _GetCache =
    cache
    
instance GetCache Cache where
  _GetCache =
    id

class SetCache a where
  _SetCache ::
    Setter' a Cache
  default _SetCache ::
    ManyCache a =>
    Setter' a Cache
  _SetCache =
    _ManyCache

instance SetCache Cache where
  _SetCache =
    id

class (FoldCache a, SetCache a) => ManyCache a where
  _ManyCache ::
    Traversal' a Cache

instance ManyCache Cache where
  _ManyCache =
    id

class (GetCache a, ManyCache a) => HasCache a where
  cache ::
    Lens' a Cache
  default cache ::
    IsCache a =>
    Lens' a Cache
  cache =
    _IsCache

instance HasCache Cache where
  cache =
    id

class (HasCache a, AsCache a) => IsCache a where
  _IsCache ::
    Iso' a Cache
    
instance IsCache Cache where
  _IsCache =
    id

instance SetCache () where
instance FoldCache () where
  _FoldCache =
    _ManyCache
instance ManyCache () where
  _ManyCache _ x =
    pure x

----


class AsReadCache a where
  _ReadCache ::
    Prism'
      a
      ()
      
instance AsReadCache () where
  _ReadCache =
    id

instance AsReadCache Cache where
  _ReadCache =
    prism'
      (\() -> ReadCache)
      (\c -> case c of
                ReadCache ->
                  Just ()
                _ ->
                  Nothing)

class AsReadWriteCache a where
  _ReadWriteCache ::
    Prism'
      a
      ()
      
instance AsReadWriteCache () where
  _ReadWriteCache =
    id

instance AsReadWriteCache Cache where
  _ReadWriteCache =
    prism'
      (\() -> ReadWriteCache)
      (\c -> case c of
                ReadWriteCache ->
                  Just ()
                _ ->
                  Nothing)

class AsNoCache a where
  _NoCache ::
    Prism'
      a
      ()
      
instance AsNoCache () where
  _NoCache =
    id

instance AsNoCache Cache where
  _NoCache =
    prism'
      (\() -> NoCache)
      (\c -> case c of
                NoCache ->
                  Just ()
                _ ->
                  Nothing)

isReadOrWriteCache ::
  (AsReadCache t, AsReadWriteCache t) =>
  t
  -> Bool
isReadOrWriteCache x =
  any (\p' -> not (isn't p' x)) [_ReadCache, _ReadWriteCache]

isWriteCache ::
  AsReadWriteCache t =>
  t
  -> Bool
isWriteCache x =
  not (isn't _ReadWriteCache x)


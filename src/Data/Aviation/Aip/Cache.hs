{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Cache(
  Cache(..)
, AsReadCache(..)
, AsReadWriteCache(..)
, AsNoCache(..)
, isReadOrWriteCache
, isWriteCache
) where

import Papa

data Cache =
  ReadCache
  | ReadWriteCache
  | NoCache
  deriving (Eq, Ord, Show)

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


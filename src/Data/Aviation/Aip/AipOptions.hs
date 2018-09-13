{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.AipOptions(
  AipOptions(..)
, aipOptionOutputDirectory
, aipOptionCache
, aipOptionLog
, aipOptionVerbose
, parserAipOptions
) where

import Control.Applicative((<*>))
import Control.Lens
import Data.Aviation.Aip.Cache(Cache(ReadCache, ReadWriteCache, NoCache))
import Data.Bool(Bool)
import Data.Eq(Eq)
import Data.Functor(fmap, (<$>))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Options.Applicative(Parser, argument, str, help, metavar, option, maybeReader, short, long, value, switch)
import Prelude(Show)
import System.FilePath(FilePath)

data AipOptions =
  AipOptions
    FilePath
    Cache
    Bool -- log
    Bool -- verbose
  deriving (Eq, Ord, Show)

aipOptionOutputDirectory ::
  Lens' AipOptions FilePath
aipOptionOutputDirectory k (AipOptions d c l v) =
  fmap (\d' -> AipOptions d' c l v) (k d)

aipOptionCache ::
  Lens' AipOptions Cache
aipOptionCache k (AipOptions d c l v) =
  fmap (\c' -> AipOptions d c' l v) (k c)

aipOptionLog ::
  Lens' AipOptions Bool
aipOptionLog k (AipOptions d c l v) =
  fmap (\l' -> AipOptions d c l' v) (k l)

aipOptionVerbose ::
  Lens' AipOptions Bool
aipOptionVerbose k (AipOptions d c l v) =
  fmap (\v' -> AipOptions d c l v') (k v)

parserAipOptions ::
  Parser AipOptions
parserAipOptions =
  AipOptions
    <$>
    Options.Applicative.argument
      str
      (
        help "AIP output directory" <>
        metavar "aip-output-directory"
      )
    <*>
    option
      (
        maybeReader
          (\s -> case s of
                   "r" -> Just ReadCache
                   "rw" -> Just ReadWriteCache
                   "no" -> Just NoCache
                   _ -> Nothing
          )
      )
      (
        short 'c' <>
        long "cache" <>
        value ReadWriteCache <>
        help "how to utilise the cache to build the AIP document tree"
      )
    <*>
    switch
      (
        long "log" <>
        short 'l' <>
        help "log to standard output"
      )
    <*>
    switch
      (
        long "verbose" <>
        short 'v' <>
        help "print the AIP document tree after download"
      )

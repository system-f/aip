{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aviation.Aip
import Papa hiding (option)
import System.Directory
import System.FilePath
import Control.Exception hiding (catch)
import Control.Monad.Catch
import Options.Applicative

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

undefined = undefined

main ::
  IO ()
main =
  let r =
        execParser
          (info (parserAipOptions <**> helper) (
            fullDesc <>
            header "aip 0.1.0 <http://www.airservicesaustralia.com/aip/aip.asp>"
          )
        )
  in  do  opts <- r
          putStrLn (show opts)
          e <- runExceptT $ run nothingAfterDownload (opts ^. aipOptionCache) (opts ^. aipOptionOutputDirectory)
          case e of
            Left e' ->
              aiplog ("network or HTTP error " ++ show e')
              -- exit code
            Right r ->
              when (opts ^. aipOptionVerbose) (putStrLn (show r))

newtype AipCon a =
  AipCon (Bool -> ExceptT ConnErrorHttp4xx IO a)

instance AipCon a ~ r =>
  Rewrapped (AipCon b) r

instance Wrapped (AipCon x) where
  type Unwrapped (AipCon x) =
    Bool
    -> ExceptT ConnErrorHttp4xx IO x
  _Wrapped' =
    iso
      (\(AipCon x) -> x)
      AipCon

instance Functor AipCon where
  fmap f (AipCon x) =
    AipCon (fmap (fmap f) x)

instance Applicative AipCon where
  pure =
    AipCon . pure . pure
  AipCon f <*> AipCon a =
    AipCon (\b -> f b <*> a b)

instance Monad AipCon where
  return =
    pure
  AipCon x >>= f =
    AipCon (\b -> x b >>= \a -> let r = f a ^. _Wrapped in r b)

instance MonadIO AipCon where
  liftIO =
    AipCon . pure . liftIO
    
newtype AfterDownload f a =
  AfterDownload
    (FilePath -> Href -> f a)

instance Functor f => Functor (AfterDownload f) where
  fmap f (AfterDownload x) =
    AfterDownload (\p h -> fmap f (x p h))

instance Applicative f => Applicative (AfterDownload f) where
  pure =
    AfterDownload . pure . pure . pure
  AfterDownload f <*> AfterDownload a =
    AfterDownload (\p h -> f p h <*> a p h)

instance Monad f => Monad (AfterDownload f) where
  return =
    pure
  AfterDownload x >>= f =
    AfterDownload (\p h -> x p h >>= \a -> let g = f a ^. _Wrapped in g p h)

instance AfterDownload f a ~ x =>
  Rewrapped (AfterDownload g k) x

instance Wrapped (AfterDownload f k) where
  type Unwrapped (AfterDownload f k) =
      FilePath
      -> Href
      -> f k
  _Wrapped' =
    iso
      (\(AfterDownload x) -> x)
      AfterDownload

nothingAfterDownload ::
  Applicative f => AfterDownload f ()
nothingAfterDownload =
  pure ()

filePathAfterDownload ::
  Applicative f => AfterDownload f FilePath
filePathAfterDownload =
  AfterDownload (\p _ -> pure p)
  
hrefAfterDownload ::
  Applicative f => AfterDownload f Href
hrefAfterDownload =
  AfterDownload (\_ h -> pure h)

type AfterDownloadAipConn a =
  AfterDownload (ExceptT ConnErrorHttp4xx IO) a

run ::
  AfterDownloadAipConn a
  -> Cache
  -> FilePath
  -> ExceptT ConnErrorHttp4xx IO AipRecords
run (AfterDownload k) cch dir =
  let catchIOException :: 
        MonadCatch m =>
        m a ->
        (IOException -> m a)
        -> m a
      catchIOException =
        catch
  in  do  x <- getAipRecords cch dir
          let h = dir </> showHash x
          de <- liftIO $ doesDirectoryExist h
          let dl = mapMOf_ _ManyHref (\c -> downloadHref h c >>= \p -> k p c) (aipPrefix x)
          catchIOException (de `unless` dl) (\e ->
            do  aiplog ("IO Exception: " ++ show e)
                liftIO $ removeDirectoryRecursive h)
          pure x

{- todo

* command line args
* tidy up cabal/nix
* write README of general flow
* split out with no dependency on unix

other lib
* tar download
* move symlinks to their own directory
* crop pdf files with pdfcrop --margins
* convert pdf to ps with pdftops

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}

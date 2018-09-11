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
import Papa
import System.Directory
import System.FilePath
import Control.Exception hiding (catch)
import Control.Monad.Catch

main ::
  IO ()
main =
  do  let basedir = "/tmp/abc"
          sha1dir = "sha1"
      e <- runExceptT $ run (pure ()) ReadWriteCache (basedir </> sha1dir)
      case e of
        Left e' ->
          aiplog ("network or HTTP error " ++ show e')
        Right r ->
          putStrLn (show r)

newtype AfterDownload a =
  AfterDownload (FilePath -> Href -> ExceptT ConnErrorHttp4xx IO a)

instance Functor AfterDownload where
  fmap f (AfterDownload x) =
    AfterDownload (\p h -> fmap f (x p h))

instance Applicative AfterDownload where
  pure =
    AfterDownload . pure . pure . pure
  AfterDownload f <*> AfterDownload a =
    AfterDownload (\p h -> f p h <*> a p h)

instance Monad AfterDownload where
  return =
    pure
  AfterDownload x >>= f =
    AfterDownload (\p h -> x p h >>= \a -> let g = f a ^. _Wrapped in g p h)

instance AfterDownload r ~ a =>
  Rewrapped (AfterDownload x) a

instance Wrapped (AfterDownload a) where
  type Unwrapped (AfterDownload a) =
      FilePath
      -> Href
      -> ExceptT ConnErrorHttp4xx IO a
  _Wrapped' =
    iso (\(AfterDownload x) -> x) AfterDownload

filePathAfterDownload ::
  AfterDownload FilePath
filePathAfterDownload =
  AfterDownload (\p _ -> pure p)
  
hrefAfterDownload ::
  AfterDownload Href
hrefAfterDownload =
  AfterDownload (\_ h -> pure h)

run ::
  AfterDownload a
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

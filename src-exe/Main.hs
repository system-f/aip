{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aviation.Aip
import Papa hiding ((.=))
import System.Directory
import System.FilePath
import Control.Exception hiding (catch)
import Control.Monad.Catch

catchIOException :: 
  MonadCatch m =>
  m a ->
  (IOException -> m a)
  -> m a
catchIOException =
  catch

main ::
  IO ()
main =
  do  let basedir = "/tmp/abc"
          sha1dir = "sha1"
      e <- runExceptT $ run (\_ _ -> pure ()) ReadWriteCache (basedir </> sha1dir)
      case e of
        Left e' ->
          aiplog ("network or HTTP error " ++ show e')
        Right r ->
          putStrLn (show r)

run ::
  (FilePath -> Href -> ExceptT ConnErrorHttp4xx IO a)
  -> Cache
  -> FilePath
  -> ExceptT ConnErrorHttp4xx IO AipRecords
run k cch dir =
  do  x <- getAipRecords cch dir
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

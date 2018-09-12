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
import System.Exit
import System.FilePath
import Control.Exception hiding (catch)
import Control.Monad.Catch
import Options.Applicative

main ::
  IO ()
main =
  run nothingAfterDownload

run ::
  AfterDownloadAipCon a
  -> IO ()
run k =
  let p =
        execParser
          (info (parserAipOptions <**> helper) (
            fullDesc <>
            header "aip 0.1.0 <http://www.airservicesaustralia.com/aip/aip.asp>"
          )
        )
  in  do  opts <- p
          let lg = (opts ^. aipOptionLog)
          e <- runExceptT ((runAip k (opts ^. aipOptionCache) (opts ^. aipOptionOutputDirectory) ^. _Wrapped) lg)
          case e of
            Left e' ->
              do  when lg (aiplog' ("network or HTTP error " ++ show e'))
                  exitWith (ExitFailure 1)
            Right r ->
              when (opts ^. aipOptionVerbose) (putStrLn (show r))

runAip ::
  AfterDownloadAipCon a
  -> Cache
  -> FilePath
  -> AipCon AipRecords
runAip (AfterDownload k) cch dir =
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

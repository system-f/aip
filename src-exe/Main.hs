{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import System.IO(print)
import Data.Aviation.Aip
import Papa hiding ((.=))
import System.Directory
import System.FilePath
import Network.HTTP
import Control.Exception hiding (catch)
import Control.Monad.Catch

basedir ::
  FilePath
basedir =
  "/tmp/abc"

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
  do  e <-  runExceptT $ 
              do  x <- getAipRecords ReadWriteCache basedir
                  let h = basedir </> hashHex (x ^. sha1) ""
                  ee <- liftIO $ doesDirectoryExist h
                  let dl = mapMOf_ _ManyHref (\k -> {- liftIO (print k) *> -} downloadHref h k) x
                  catchIOException (ee `unless` dl) (\_ -> liftIO $ removeDirectoryRecursive h)
      print e

aipPrefix ::
  ASetter a b String String
  -> a
  -> b
aipPrefix a =
  let p = "/aip/" in
  a %~ (bool <$> (p ++) <*> id <*> isPrefixOf p)

downloadHref ::
  FilePath
  -> Href
  -> AipConn () 
downloadHref d hf =
  let hf' = aipPrefix _Wrapped hf
  in  do  
          let q = aipRequestGet hf' ""
          auth <- getAuth q
          c <- liftIO $ openStream (host auth) 80
          r <- doRequest' (normalizeRequest defaultNormalizeRequestOptions q) c
          let (j, k) = splitFileName (hf' ^. _Wrapped)
          let ot = d </> dropWhile isPathSeparator j
          liftIO $
            do  createDirectoryIfMissing True ot
                LazyByteString.writeFile (ot </> k) r
                close c

{- todo

* All As* requires Many* =>
* logging
* command line args
* tidy up cabal/nix

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}

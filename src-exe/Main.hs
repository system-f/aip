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

main ::
  IO ()
main =
  do  e <-  runExceptT $ 
              do  x <- getAipRecords ReadWriteCache "/tmp/abc"
                  mapMOf_ _ManyHref (\k -> liftIO (print k) *> downloadHref k) x
      print e

basedir ::
  FilePath
basedir =
  "/tmp/def"

downloadHref ::
  Href
  -> AipConn () 
downloadHref hf =
  let hf' = bool (_Wrapped %~ ("/aip/" ++)) id ("/aip/" `isPrefixOf` (hf ^. _Wrapped)) $ hf
  in  do  
          let q = aipRequestGet hf' ""
          auth <- getAuth q
          c <- liftIO $ openStream (host auth) 80
          r <- doRequest' (normalizeRequest defaultNormalizeRequestOptions q) c
          let (j, k) = splitFileName (hf' ^. _Wrapped)
          let ot = basedir </> dropWhile isPathSeparator j
          liftIO $
            do  createDirectoryIfMissing True ot
                LazyByteString.writeFile (ot </> k) r
                close c

{- todo

* fix HTTP downloads
* download function
  * only write cache if succeeds
* logging
* command line args

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LazyByteString(ByteString, writeFile)
import System.IO(print)
import Data.Aviation.Aip
import Papa hiding ((.=))
import System.Directory
import System.FilePath

main ::
  IO ()
main =
  do  e <-  runExceptT $ 
              do  x <- getAipRecords ReadWriteCache "/tmp/abc"
                  mapMOf_ _ManyHref (\k -> liftIO (print k) *> downloadHref k) x
      print e
   
basedir =
  "/tmp/def"
 
downloadHref hf =
  do  r <- doGetRequest hf "" :: ExceptT ConnErrorHttp4xx IO LazyByteString.ByteString
      let (j, k) = splitFileName (hf ^. _Wrapped)
      let o = basedir </> j
      liftIO $ createDirectoryIfMissing True o
      liftIO $ LazyByteString.writeFile (o </> k) r

{-
Href "aip.asp?pg=20&vdate=16AUG2018&ver=1"
Href "current/aip/complete_16AUG2018.pdf"
Href "current/aip/general_16AUG2018.pdf"
Href "current/aip/enroute_16AUG2018.pdf"
Href "current/aip/aerodrome_16AUG2018.pdf"
Href "current/aip/cover_16AUG2018.pdf"
Href "aip.asp?pg=60&vdate=24-MAY-2018&ver=1"
Href "aip.asp?pg=60&vdate=24-MAY-2018&sect=ERCHigh&ver=1"
Href "/aip/current/aipchart/erch/erch1_24MAY2018.pdf"
-}

undefined = undefined
{- todo

* library support
  * lens
  * Plated
* download function
  * only write cache if succeeds
* logging
* command line args

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com/services/aeronautical-information-and-management-services/electronic-data/

-}

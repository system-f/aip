{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import System.IO(print)
import Data.Aviation.Aip
import Papa hiding ((.=))
import System.Directory
import System.FilePath

{-
*** Exception: /tmp/def/aip/current/dap/AeroProcChartsTOC.htm: openBinaryFile: resource exhausted (Too many open files)
-}
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
  let hf' = bool (_Wrapped %~ ("/aip/" ++)) id ("/aip/" `isPrefixOf` (hf ^. _Wrapped)) $ hf
  in  do  r <- doGetRequest hf' ""
          let (j, k) = splitFileName (hf' ^. _Wrapped)
          let o = basedir </> dropWhile isPathSeparator j
          liftIO $ createDirectoryIfMissing True o
          liftIO $ writeFile' (o </> k) r

writeFile' p x =
  LazyByteString.writeFile p x `catch` \e -> print (e :: IOException) <* getLine

{- todo

* download function
  * only write cache if succeeds
* logging
* command line args

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}

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

main ::
  IO ()
main =
  do  e <-  runExceptT $ 
              do  x <- getAipRecords ReadWriteCache "/tmp/abc"
                  mapMOf_ _ManyHref downloadHref x
      print e
    
downloadHref hf =
  do  r <- doGetRequest hf "" :: ExceptT ConnErrorHttp4xx IO LazyByteString.ByteString
      let w = r
      liftIO $ print hf

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

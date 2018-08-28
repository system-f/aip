{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.Trans.Except
import System.IO(print)
import Data.Aviation.Aip
import Papa hiding ((.=))

main ::
  IO ()
main =
  do  x <- runExceptT $ getAipRecords ReadWriteCache "/tmp/abc"
      print x

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

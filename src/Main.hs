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

* newtype Href = Href String
* trim spaces from tags
* fix some relative hrefs in tags
* library support
  * lens
  * Plated
* download function
  * only write cache if succeeds
* logging
* command line args


-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip(run, nothingAfterDownload)
import System.IO(IO)

main ::
  IO ()
main =
  run nothingAfterDownload

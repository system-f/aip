{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip(run, nothingAfterDownload)
import Papa

main ::
  IO ()
main =
  run nothingAfterDownload

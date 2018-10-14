{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip
import System.IO(IO)

main ::
  IO ()
main =
  run defaultPerHref defaultOnAipRecords

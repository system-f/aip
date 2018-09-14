{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip(run, downloadHref)
import System.IO(IO)

main ::
  IO ()
main =
  run downloadHref

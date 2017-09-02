{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip.AipDocuments(distributeAipDocuments)
import System.Environment(getArgs)
import System.IO(IO, hPutStrLn, stderr)
import Papa

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        adir:ldir:_ ->
          void (distributeAipDocuments adir ldir)
        _ ->
          hPutStrLn stderr "<aip-directory> <log-directory>"

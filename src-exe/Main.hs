{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip -- (run, downloadHref)
import System.IO(IO)
import Prelude
import Control.Monad.IO.Class
import Data.Time
import Data.Bool
import Data.Maybe
import Control.Lens
import System.Directory
import System.FilePath
import System.Posix.Files

main ::
  IO ()
main =
  run (downloadHref >>= \z -> ioPerHref (\h d d' -> print (z, h, d, d'))) printOnAipRecords

--  (
--    ("/tmp/llll/aip/current/aip/complete_16AUG2018.pdf"
-- ,  Href "/aip/current/aip/complete_16AUG2018.pdf"
-- ,  "/tmp/llll"
-- ,  "/tmp/llll/sha1/2d057180bd846bc5f717f47cdb892292b347aa52")
--   )

timeDirectory ::
  UTCTime
  -> FilePath
timeDirectory (UTCTime dy f) =
  let (y, m, d) =
        toGregorian dy
      xx n =
        bool id ('0':) (n < 10) (show n)
  in  concat
        [
          show y
        , "-"
        , xx m
        , "-"
        , xx d
        , "."
        , show (round (f * 1000) :: Integer)
        ]

latest :: PerHrefAipCon ()
latest =
  do  d <- basedirPerHref
      w <- downloaddirPerHref
      -- delete existing latest
      liftIO $ linkRelative d (splitPath w) ["latest"]

linkRelative ::
  FilePath
  -> [FilePath]
  -> [FilePath]
  -> IO ()
linkRelative base t fr =
  let fr' = fromMaybe [] (fr ^? _init)
      lk = (".." <$ fr') ++ t
  in  do  createDirectoryIfMissing True (joinPath (base : fr'))
          createSymbolicLink (joinPath lk) (joinPath (base : fr))

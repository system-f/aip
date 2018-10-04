{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Applicative
import Data.Aviation.Aip -- (run, downloadHref)
import System.IO(IO)
import Prelude
import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Data.Bool
import Data.Foldable
import Data.Maybe
import Control.Exception
import Control.Lens
import System.Directory
import System.Directory(createDirectoryLink)
import System.IO.Error
import System.FilePath
import System.Posix.Files

main ::
  IO ()
main =
  run (downloadHref >>= \z -> ioPerHref (\h d d' -> print (z, h, d, d')))
      ({- printOnAipRecords *> -} (latestLink >>= \l -> timeLink >>= \t -> liftIO (print (l, t))))

latestLink ::
  OnAipRecordsIO [FilePath]
latestLink =
  downloaddirOnAipRecords >>=
    liftIO .
      mapM (\p ->  let lt = takeDirectory p </> "latest"
                        in  do  removeFileIfExists lt
                                createDirectoryLink p lt
                                pure lt) . toList

timeLink ::
  OnAipRecordsIO [FilePath]
timeLink =
  let timeDirectory ::
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
  in  do  d <- basedirOnAipRecords
          p <- downloaddirOnAipRecords
          let td = d </> "time"
          liftIO $ createDirectoryIfMissing True td
          t <- aipRecordsTimesOnAipRecords
          let links =
                do  t' <- t
                    p' <- toList p
                    pure (td </> timeDirectory t', p')
          liftIO $
            mapM (\b ->
              let (u, v) = doRelative b d
              in  do  removeFileIfExists u
                      createDirectoryLink v u
                      pure u) links
            
-- |
--
-- >>> doRelative ("/a/b/c/d/e", "/a/b/c") "/a/b"
-- ("/a/b/c/d/e","../../c")
--
-- >>> doRelative ("/a/b/c/d/e", "/a/b/c/x") "/a/b"
-- ("/a/b/c/d/e","../../c/x")
--
-- >>> doRelative ("/a/b/c/d/e", "/a/b/c/x") "/a"
-- ("/a/b/c/d/e","../../../b/c/x")
doRelative ::
  (FilePath, FilePath)
  -> FilePath
  -> (FilePath, FilePath)
doRelative x a =
  let (q, r) = (both %~ makeRelative a) x
      q' = joinPath . reverse . drop 1 . set (_tail . traverse) ".." . reverse . splitPath $ q
  in  (a </> q, q' </> r)

removeFileIfExists ::
  FilePath
  -> IO ()
removeFileIfExists fileName =
  removeFile fileName `catch` (liftA2 unless isDoesNotExistError throwIO)

{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Aip -- (run, downloadHref)
import System.IO(IO)
import Prelude
import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Data.Bool
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
  run (downloadHref >>= \z -> ioPerHref (\h d d' -> print (z, h, d, d'))) ({- printOnAipRecords *> -} (latestLink *> timeLink))

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

removeFileIfExists ::
  FilePath
  -> IO ()
removeFileIfExists fileName =
  removeFile fileName `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))

latestLink ::
  OnAipRecordsIO ()
latestLink =
  downloaddirOnAipRecords >>=
    liftIO .
      mapM_ (\p ->  let lt = takeDirectory p </> "latest"
                    in  do  removeFileIfExists lt
                            createDirectoryLink p lt)

timeLink ::
  OnAipRecordsIO ()
timeLink =
  do  d <- basedirOnAipRecords
      p <- downloaddirOnAipRecords
      let td = d </> "time"
      liftIO $ createDirectoryIfMissing True td
      t <- aipRecordsTimesOnAipRecords
      let ttt = fmap (\t' -> td </> timeDirectory t') t
      let ttttt =
            do  t' <- t
                
                fmap (\t' -> td </> timeDirectory t') t
      -- liftIO $ mapM_ (\tttt -> mapM_ (\p' -> createDirectoryLink p' tttt) p) ttt
      liftIO $ mapM_ (\tttt -> mapM_ (\p' -> linkRelative d (splitPath p') (splitPath tttt)) p) ttt

linkRelative ::
  FilePath
  -> [FilePath]
  -> [FilePath]
  -> IO ()
linkRelative base t fr =
  let fr' = fromMaybe [] (fr ^? _init)
      lk = (".." <$ fr') ++ t
  in  do  createDirectoryIfMissing True (joinPath (base : fr'))
          createDirectoryLink (joinPath lk) (joinPath (base : fr))

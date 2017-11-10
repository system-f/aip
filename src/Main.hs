{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Aviation.Aip.AipDocuments(distributeAipDocuments)
import Data.Time(UTCTime(utctDay, utctDayTime), TimeOfDay(TimeOfDay), toGregorian, timeToTimeOfDay, getCurrentTime)
import System.Environment(getArgs)
import System.IO(IO, hPutStrLn, stderr)
import Sys.Exit(CreateProcess, ExitCodeM, procIn, createMakeWaitProcessM, exit)
import System.Directory(listDirectory, doesDirectoryExist)
import System.FilePath((</>), splitFileName)
import Papa

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        adir:_ ->
          do  t <- getCurrentTime
              let u = time t
                  d = adir </> u ++ "UTC"
              void (distributeAipDocuments (d </> "aip") (d </> "log"))
              exit $ do   createMakeWaitProcessM . linkLatest adir $ u
                          tarDirectories d

        _ ->
          hPutStrLn stderr "<aip-output-directory>"

directories ::
  FilePath
  -> IO [FilePath]
directories p =
  listDirectory p >>= \ds -> filterM doesDirectoryExist ((p </>) <$> ds)

tarDirectories ::
  FilePath
  -> ExitCodeM IO
tarDirectories =
  let tarDirectories' p =
        let tarDirectory d =
              let (e, g) = splitFileName d
              in  procIn d "tar"
                    [
                      "-C"
                    , e
                    , "-zcvf"
                    , d ++ ".tar.gz"
                    , g
                    ]
        in  do  ds <- lift (directories p)
                mapM_ (\d -> createMakeWaitProcessM (tarDirectory d) >> tarDirectories' d) ds
  in  tarDirectories'

linkLatest ::
  FilePath
  -> String
  -> CreateProcess
linkLatest d t =
  procIn d "ln"
    [
      "-f"
    , "-s"
    , "-n"
    , t
    , "latest"
    ]

time ::
  UTCTime
  -> String
time t =
  let show2 = let s2 [x] = ['0', x]
                  s2 x = x
              in s2 . show
      (y, m, d) = toGregorian (utctDay t)
      TimeOfDay h n s = timeToTimeOfDay (utctDayTime t)
  in concat [show y, show2 m, show2 d, "-", show2 h, show2 n, show2 (floor s)]

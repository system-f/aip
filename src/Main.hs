{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Exitcode(ExitcodeT0, ExitcodeT, fromExitCode, runExitcode)
import Control.Monad((>=>))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Aviation.Aip.AipDocuments(distributeAipDocuments)
import Data.Time(UTCTime(utctDay, utctDayTime), TimeOfDay(TimeOfDay), toGregorian, timeToTimeOfDay, getCurrentTime)
import System.Directory(listDirectory, doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath((</>), takeDirectory, splitFileName, takeExtension)
import System.IO(IO, hPutStrLn, stderr)
import System.Process(CreateProcess(cwd), ProcessHandle, createProcess, waitForProcess, proc)
import Papa

createProcessHandle ::
  CreateProcess
  -> IO ProcessHandle
createProcessHandle =
  fmap (view _4) . createProcess

createMakeWaitProcess ::
  CreateProcess
  -> ExitcodeT0 IO
createMakeWaitProcess c =
  fromExitCode $
    do  mapM_ (createDirectoryIfMissing True) (cwd c)
        (createProcessHandle >=> waitForProcess) c

procIn ::
  FilePath -- ^ the working directory
  -> FilePath
  -> [String]
  -> CreateProcess
procIn dir p s =
  (\q -> q { cwd =  Just dir}) -- todo lens
    (proc p s)

exit ::
  ExitcodeT0 IO
  -> IO ()
exit e =
  toExitCode e >>= exitWith

-- belongs in exitcode
toExitCode ::
  Functor f =>
  ExitcodeT f a
  -> f ExitCode
toExitCode e =
  either ExitFailure (const ExitSuccess) <$> runExitcode e

----

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        adir:_ ->
          do  t <- getCurrentTime
              let u = time t ++ "UTC"
                  d = adir </> u
              void (distributeAipDocuments (d </> "aip") (d </> "log"))
              exit $ do   createMakeWaitProcess . linkLatest adir $ u
                          tarDirectories d (d </> "download")
                          m <- lift (pdffiles (d </> "aip"))
                          mapM_ (\(dty, ext, n) -> convert' dty (d </> "aip") ext n) ((,,) <$> [100, 250] <*> ["png"] <*> m)
        _ ->
          hPutStrLn stderr "<aip-output-directory>"

convert' ::
  Int
  -> FilePath
  -> String
  -> FilePath
  -> ExitcodeT0 IO
convert' dty d ext p =
  let ot = takeDirectory d </> "convert" </> p ++ ".density" ++ show dty ++ "." ++ ext
  in  do  lift (createDirectoryIfMissing True (takeDirectory ot))
          createMakeWaitProcess (convert dty d p ot)

convert ::
  Int
  -> FilePath -- in directory
  -> FilePath -- pdf
  -> FilePath -- image
  -> CreateProcess
convert dty d p q =
  procIn d "convert"
    [
      "-density"
    , show dty
    , p
    , q
    ]

pdffiles ::
  FilePath
  -> IO [FilePath]
pdffiles v =
  let pdffiles' ::
        FilePath
        -> FilePath
        -> IO [FilePath]
      pdffiles' q p =
        do  x <- listDirectory (q </> p)
            let x' = ((p </>) <$> x)
            g <- filterM (\f -> (&& takeExtension (q </> f) == ".pdf") <$> doesFileExist (q </> f)) x'
            d <- filterM (\f -> doesDirectoryExist (q </> f)) x'
            e <- mapM (pdffiles' q) d
            pure (g ++ concat e)
  in pdffiles' v ""

directories ::
  FilePath
  -> IO [FilePath]
directories p =
  listDirectory p >>= \ds -> filterM (\d -> doesDirectoryExist (p </> d)) ds

tarDirectories ::
  FilePath
  -> FilePath
  -> ExitcodeT0 IO
tarDirectories d1 d2 =
  let tarDirectories' r s =
        let tarDirectory ::
              FilePath
              -> FilePath
              -> CreateProcess
            tarDirectory d e =
              let (k, g) = splitFileName d
              in  procIn d "tar"
                    [
                      "-C"
                    , k
                    , "-zcvf"
                    , e ++ ".tar.gz"
                    , g
                    ]
            tarDirectory' ::
              FilePath
              -> FilePath
              -> ExitcodeT0 IO
            tarDirectory' d e =
              do  lift (createDirectoryIfMissing True (takeDirectory e))
                  p <- lift (doesFileExist (e ++ ".tar.gz"))
                  p `unless` createMakeWaitProcess (tarDirectory d e)
        in  do  ds <- lift (directories r)
                mapM_ (\d -> 
                  let r' = r </> d
                      s' = s </> d
                  in  do  (r' == s) `unless` tarDirectory' r' s'
                          tarDirectories' r' s'
                      ) ds
  in  tarDirectories' d1 d2

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

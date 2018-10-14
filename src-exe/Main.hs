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
import Data.Char
import Data.List
import Data.Foldable
import Control.Exception hiding (catch)
import Control.Lens
import System.Directory
import System.Directory(createDirectoryLink)
import System.IO.Error
import System.Exit
import System.FilePath
import System.Process
import Control.Monad.Catch(MonadCatch(catch))

main ::
  IO ()
main =
  run (downloadHref >>= \z -> ioPerHref (\h d d' -> print (z, h, d, d'))) (latestLink >>= \l -> timeLink >>= \t -> removeDate >>= \r -> archive (latestLinkList l) >>= \c -> liftIO (print (l, t, c)))

removeDate ::
  OnAipRecordsIO [FilePath]
removeDate =
  let linkHref ::
        Either IOException FilePath
        -> Href
        -> IO (Maybe FilePath)
      linkHref d (Href h) =
        let ms = 
              do  d'     <- d ^? _Right
                  (a, r) <- unsnoc . splitDirectories . dropWhile isPathSeparator $ h
                  pure (d', a, r)
        in  mapM (\(d', a, j) ->
              do  let i = joinPath a
                  let i' = d' </> "nodate" </> i
                  let link = i' </> removeDateFilePath j
                  mkdir i'
                  _ <-  removeIfExistsThenCreateDirectoryLink
                          link
                          (".." </> dotdot a </> d' </> i </> j)
                  pure link
                  ) ms
  in  do  r <- prefixedAipRecordsOnAipRecords
          d <- downloaddirOnAipRecords
          z <- liftIO $ traverse (linkHref d) (toListOf (_Right . _ManyHref) r)
          pure (z ^.. folded . _Just)

getSymbolicLinkTarget' ::
  FilePath
  -> IO (Maybe FilePath)
getSymbolicLinkTarget' x =
  let catchIOException :: 
        MonadCatch m =>
        m a ->
        (IOException -> m a)
        -> m a
      catchIOException =
        catch
  in  catchIOException (Just <$> getSymbolicLinkTarget x) (const (pure Nothing))

latestLinkList ::
  Either IOException (Maybe FilePath, FilePath, FilePath)
  -> [FilePath]
latestLinkList g =
  maybe [] pure (g ^? _Right . _2)
  
latestLink ::
  OnAipRecordsIO (Either IOException (Maybe FilePath, FilePath, FilePath))
latestLink =
  downloaddirOnAipRecords >>=
    mapM (\p -> let lt = takeDirectory p </> "latest"
                in  do  z <- liftIO (removeIfExistsThenCreateDirectoryLink lt p)
                        b <- basedirOnAipRecords
                        let bt = b </> "latest"
                        _ <- liftIO (removeIfExistsThenCreateDirectoryLink bt lt)
                        pure (z, bt, lt))

followLinks ::
  FilePath
  -> IO FilePath
followLinks p =
  do  r <- getSymbolicLinkTarget' p
      case r of
        Nothing ->
          pure p
        Just s ->
          followLinks s

archive ::
  [FilePath]
  -> OnAipRecordsIO (Either IOException [(FilePath, ExitCode)])
archive x =
  let system' ::
        [String]
        -> IO ExitCode
      system' s =
        do  let s' = intercalate " " s
            e <- system s'
            putStrLn s'
            pure e
      quote ::
        String
        -> String
      quote w =
        '"' : w ++ "\""
      targz ::
        Traversable f =>
        f FilePath
        -> OnAipRecordsIO [(FilePath, ExitCode)]
      targz d =
        liftIO $
          (^.. folded . _Just) <$>
          mapM (\d' ->
            do  d'' <- followLinks d'
                let (b', z') = splitFileName d''
                    (b, z) = splitFileName d'
                    arch = b </> z ++ ".tar.gz"
                a <- doesFileExist arch
                if a
                  then
                    pure Nothing
                  else
                    do  t <- doesDirectoryExist d'
                        if t
                          then
                            do  k <- system'
                                      [
                                        "tar"
                                      , "--transform"
                                      , quote ("s/" ++ z' ++ "/" ++ z ++ "/")
                                      , "-C"
                                      , b'
                                      , "-czvf"
                                      , quote arch
                                      , z'
                                      ]
                                pure (Just (arch, k))
                          else
                            pure Nothing
          ) d
  in  do  d <- downloaddirOnAipRecords
          mapM (\d' -> targz (x ++
                                [
                                  d'
                                , d' </> "aip" </> "current"
                                , d' </> "aip" </> "current" </> "aip"
                                , d' </> "aip" </> "current" </> "aipchart"
                                , d' </> "aip" </> "current" </> "aipchart" </> "erch"
                                , d' </> "aip" </> "current" </> "aipchart" </> "ercl"
                                , d' </> "aip" </> "current" </> "aipchart" </> "pca"
                                , d' </> "aip" </> "current" </> "aipchart" </> "tac"
                                , d' </> "aip" </> "current" </> "aipchart" </> "vnc"
                                , d' </> "aip" </> "current" </> "aipchart" </> "vtc"
                                , d' </> "aip" </> "current" </> "dap"
                                , d' </> "aip" </> "current" </> "ersa"
                                , d' </> "aip" </> "current" </> "sup"
                                , d' </> "aip" </> "current" </> "SUP_AIP_Summary"
                                , d' </> "aip" </> "pending"
                                , d' </> "aip" </> "pending" </> "aip"
                                , d' </> "aip" </> "pending" </> "aipchart"
                                , d' </> "aip" </> "pending" </> "aipchart" </> "erch"
                                , d' </> "aip" </> "pending" </> "aipchart" </> "ercl"
                                , d' </> "aip" </> "pending" </> "aipchart" </> "pca"
                                , d' </> "aip" </> "pending" </> "aipchart" </> "tac"
                                , d' </> "aip" </> "pending" </> "aipchart" </> "vnc"
                                , d' </> "aip" </> "pending" </> "aipchart" </> "vtc"
                                , d' </> "aip" </> "pending" </> "dap"
                                , d' </> "aip" </> "pending" </> "ersa"
                                , d' </> "aip" </> "pending" </> "sup"
                                , d' </> "aip" </> "pending" </> "SUP_AIP_Summary"
                                ])) d

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
          liftIO $ mkdir td
          t <- aipRecordsTimesOnAipRecords
          let links =
                do  t' <- t
                    p' <- toList p
                    pure (td </> timeDirectory t', p')
          liftIO $
            mapM (\b ->
              let (u, v) = doRelative b d
              in  do  _ <- removeIfExistsThenCreateDirectoryLink u v
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

removeIfExistsThenCreateDirectoryLink ::
  FilePath
  -> FilePath
  -> IO (Maybe FilePath)
removeIfExistsThenCreateDirectoryLink u v =
  let u' = windows_replace u
  in  do  r <- getSymbolicLinkTarget' u'
          removeFileIfExists u'
          createDirectoryLink (windows_replace v) u'
          pure r

dotdot ::
  [a]
  -> FilePath
dotdot a =
  joinPath (".." <$ a)

mkdir ::
  String
  -> IO ()
mkdir d =
  createDirectoryIfMissing True (windows_replace d) 

removeFileIfExists ::
  FilePath
  -> IO ()
removeFileIfExists fileName =
  removeFile fileName `catch` (liftA2 unless isDoesNotExistError throwIO)

removeDateFilePath ::
  FilePath
  -> FilePath
removeDateFilePath x =
  case reverse x of
    (ext3:ext2:ext1:'.':y4:y3:y2:y1:m3:m2:m1:d2:d1:'_':r) ->
      if and [all isDigit [y4,y3,y2,y1], all isUpper [m3,m2,m1], all isDigit [d2,d1]]
        then
          reverse r ++ '.':ext1:ext2:[ext3]
        else
          x
    _ ->
      x


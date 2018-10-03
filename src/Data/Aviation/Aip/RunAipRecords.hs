module Data.Aviation.Aip.RunAipRecords(
  run
) where

import Control.Applicative(pure, (<**>))
import Control.Category((.))
import Control.Exception(IOException)
import Control.Lens
import Control.Monad(when, unless)
import Control.Monad.Catch(MonadCatch(catch))
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Except(runExceptT)
import Data.Aviation.Aip.AipCon(AipCon)
import Data.Aviation.Aip.AipOptions(parserAipOptions, aipOptionLog, aipOptionCache, aipOptionOutputDirectory)
import Data.Aviation.Aip.AipRecords(AipRecords, getAipRecords)
import Data.Aviation.Aip.Cache(Cache)
import Data.Aviation.Aip.Href(ManyHref(_ManyHref), aipPrefix)
import Data.Aviation.Aip.Log(aiplog, aiplog')
import Data.Aviation.Aip.OnAipRecords
import Data.Aviation.Aip.PerHref(PerHref(PerHref), PerHrefAipCon)
import Data.Aviation.Aip.SHA1(_GetSHA1, strHash)
import Data.Either(Either(Left, Right))
import Data.Function(($))
import Data.Semigroup(Semigroup((<>)))
import Options.Applicative(execParser, info, helper, fullDesc, header)
import Prelude(show)
import System.Directory(doesDirectoryExist, removeDirectoryRecursive)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.FilePath(FilePath, (</>))
import System.IO(IO)

run ::
  PerHrefAipCon ()
  -> OnAipRecordsIO ()
  -> IO ()
run k (OnAipRecords l) =
  let writeAip ::
        PerHrefAipCon ()
        -> Cache
        -> FilePath
        -> AipCon (Either IOException (FilePath, AipRecords))
      writeAip (PerHref w) cch dir =
        let catchIOException :: 
              MonadCatch m =>
              m a ->
              (IOException -> m a)
              -> m a
            catchIOException =
              catch
        in  do  let dir' = dir </> "sha1"
                x <- getAipRecords cch dir'
                let h = dir' </> view (_GetSHA1 . re strHash) x
                de <- liftIO $ doesDirectoryExist h
                let dl = mapMOf_ _ManyHref (\c -> w c dir h) (aipPrefix x)
                catchIOException
                  (do de `unless` dl
                      pure (Right (h, x)))
                  (\e ->
                          do  aiplog ("IO Exception: " <> show e)
                              liftIO $ removeDirectoryRecursive h
                              pure (Left e))
      p =
        execParser
          (info (parserAipOptions <**> helper) (
            fullDesc <>
            header "aip 0.1.0 <http://www.airservicesaustralia.com/aip/aip.asp>"
          )
        )
  in  do  opts <- p
          let lg = (opts ^. aipOptionLog)
          let dir = opts ^. aipOptionOutputDirectory
          e <- runExceptT ((writeAip k (opts ^. aipOptionCache) dir ^. _Wrapped) lg)
          case e of
            Left e' ->
              do  when lg (aiplog' ("network or HTTP error " <> show e'))
                  exitWith (ExitFailure 1)
            Right r ->
              l r dir

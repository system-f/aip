{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class(MonadIO(liftIO))
import Data.Aviation.Aip.AipDocuments(writeAipDocuments)
import Control.Monad.Trans.Except(runExceptT)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.FilePath((</>))
import System.IO(IO, Handle, IOMode(AppendMode), withFile, hPutStrLn, stderr)
import Papa

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        adir:ldir:_ ->
          let wlogfile ::
                MonadIO m =>
                FilePath
                -> (Handle -> IO a)
                -> m a
              wlogfile f =
                liftIO . withFile (ldir </> f) AppendMode
              ex = 
                    do  liftIO (mapM_ (createDirectoryIfMissing True) [adir, ldir])
                        wlogfile "err.log" (\herr ->
                          wlogfile "out.log" (\hout ->
                            do  p <- runExceptT (writeAipDocuments herr hout adir)
                                mapM_ (\qs -> appendFile (ldir </> "aip") (qs >>= \q -> q ++ "\n")) p))                        
          in  void (runExceptT ex)
        _ ->
          hPutStrLn stderr "<aip-directory> <log-directory>"

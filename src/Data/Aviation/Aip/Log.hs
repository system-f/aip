{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Log(
  aiplog
, aiplog'
) where

import Control.Monad.IO.Class(MonadIO(liftIO))
import Papa
import System.IO(hPutStrLn, stderr)

aiplog ::
  MonadIO f =>
  String
  -> f ()
aiplog =
  liftIO . aiplog'

aiplog' ::
  String
  -> IO ()
aiplog' =
  hPutStrLn stderr

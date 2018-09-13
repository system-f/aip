{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Log(
  aiplog
, aiplog'
) where

import Control.Category((.))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Data.String(String)
import System.IO(hPutStrLn, stderr, IO)

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

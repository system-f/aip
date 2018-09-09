{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Log(
  aiplog
) where

import Control.Monad.IO.Class
import Papa

aiplog ::
  MonadIO f =>
  String
  -> f ()
aiplog =
  liftIO . putStrLn

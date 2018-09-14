{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Log(
  aiplog
, aiplog'
) where

import Control.Monad(when)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Data.Aviation.Aip.AipCon
import Data.String(String)
import System.IO(hPutStrLn, stderr, IO)

aiplog ::
  String
  -> AipCon ()
aiplog s =
  do  b <- islog
      when b (liftIO (aiplog' s))

aiplog' ::
  String
  -> IO ()
aiplog' =
  hPutStrLn stderr

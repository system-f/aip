{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Aip.AipDocument(
  AipDocument(..)
, requestAipDocument
, HasAipDocument(..)
) where

import Data.ByteString(ByteString)
import qualified Data.ByteString as ByteString(writeFile)
import Network.HTTP(Request, HasHeaders(getHeaders, setHeaders), rqURI)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(takeDirectory)
import System.IO(Handle, hPutStrLn)
import Control.Monad.Trans.Except(runExceptT)
import Data.Aviation.Aip.ConnErrorHttp4xx(ConnErrorHttp4xx(IsConnError, Http4xx))
import Data.Aviation.Aip.HttpRequest(doRequest)
import Papa

data AipDocument ty =
  AipDocument {
    _aipRequest ::
      Request ty
  , _aipDocumentPath ::
      FilePath
  } deriving Show

makeClassy ''AipDocument

instance HasHeaders (AipDocument ty) where
  getHeaders (AipDocument r _) =
    getHeaders r
  setHeaders (AipDocument r p) h =
    AipDocument (setHeaders r h) p

requestAipDocument ::
   Handle -- ^ log error
  -> Handle -- ^ log out
  -> AipDocument ByteString
  -> IO (Maybe FilePath)
requestAipDocument err outlog (AipDocument r p) =
  do  z <- runExceptT (doRequest r)
      case z of
        Left e ->
          let logmsg (IsConnError ee) =
                show ee
              logmsg (Http4xx x y) =
                concat
                  [
                    "HTTP error 4"
                  , show x
                  , show y
                  , "    "
                  , show (rqURI r)
                  ]
          in  Nothing <$ hPutStrLn err (logmsg e)
        Right s ->
          do  createDirectoryIfMissing True . takeDirectory $ p
              ByteString.writeFile p $ s
              hPutStrLn outlog (concat ["uri ", show (rqURI r), "    created file ", p])
              pure (Just p)

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.HttpRequest(
  aipRequestGet
, aipRequestPost
, aipRequestMethod
, doRequest
, doRequest'
, doGetRequest
, doPostRequest
, requestAipContents
, downloadHref
) where

import System.Directory(createDirectoryIfMissing)
import System.FilePath(splitFileName, isPathSeparator, (</>))
import Control.Monad.IO.Class(liftIO)
import Network.HTTP(HandleStream, getAuth, openStream, host, normalizeRequest, defaultNormalizeRequestOptions, close)
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import Control.Monad.Trans.Except(ExceptT(ExceptT))
import Data.Aviation.Aip.AipCon(AipCon(AipCon))
import Data.Aviation.Aip.Log(aiplog)
import Data.Aviation.Aip.ConnErrorHttp4xx(ConnErrorHttp4xx(IsConnError, Http4xx))
import Data.Aviation.Aip.Href(Href(Href))
import Network.HTTP(HStream, Request, RequestMethod(GET, POST), mkRequest, setRequestBody, simpleHTTP, simpleHTTP_, rspCode, rspBody)
import Network.BufferType(BufferType)
import Network.URI(URI(URI), URIAuth(URIAuth))
import Papa

aipRequestGet ::
  BufferType ty =>
  Href
  -> String
  -> Request ty
aipRequestGet =
  aipRequestMethod GET

aipRequestPost ::
  BufferType ty =>
  Href
  -> String
  -> Request ty
aipRequestPost =
  aipRequestMethod POST

aipRequestMethod ::
  BufferType ty =>
  RequestMethod
  -> Href
  -> String
  -> Request ty
aipRequestMethod m (Href s) z =
  let s' = bool ("/aip/" ++ s) s ("/aip/" `isPrefixOf` s)
  in  mkRequest m (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) s' z "")

doRequest ::
  HStream a =>
  Request a
  -> AipCon a
doRequest r =
  AipCon . const .
  ExceptT $
    do  x <- simpleHTTP r
        pure $
          case x of
            Left e ->
              Left (IsConnError e)
            Right c ->
              let (r1, r2, r3) = rspCode c
              in  if r1 == 4 then
                    Left (Http4xx r2 r3)
                  else
                    Right (rspBody c)

doRequest' ::
  HStream a =>
  Request a
  -> HandleStream a
  -> AipCon a
doRequest' r h =
  AipCon . const .
  ExceptT $
    do  x <- simpleHTTP_ h r
        pure $
          case x of
            Left e ->
              Left (IsConnError e)
            Right c ->
              let (r1, r2, r3) = rspCode c
              in  if r1 == 4 then
                    Left (Http4xx r2 r3)
                  else
                    Right (rspBody c)

doGetRequest ::
  HStream a =>
  Href
  -> String
  -> AipCon a
doGetRequest s z =
  doRequest (aipRequestGet s z)

doPostRequest ::
  HStream a =>
  Href
  -> String
  -> AipCon a
doPostRequest s z =
  doRequest (aipRequestPost s z)

requestAipContents ::
  AipCon String
requestAipContents =
  let r = setRequestBody
            (aipRequestPost (Href "aip.asp") "?pg=10")
            ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")
  in  doRequest r

downloadHref ::
  FilePath
  -> Href
  -> AipCon FilePath
downloadHref d hf =
  do  let q = aipRequestGet hf ""
      aiplog ("making request for aip document " ++ show q)
      auth <- getAuth q
      aiplog ("making request for aip document with auth " ++ show auth)
      c <- liftIO $ openStream (host auth) 80
      r <- doRequest' (normalizeRequest defaultNormalizeRequestOptions q) c
      let (j, k) = splitFileName (hf ^. _Wrapped)
      let ot = d </> dropWhile isPathSeparator j
      aiplog ("output directory for aip document " ++ ot)
      do  liftIO $ createDirectoryIfMissing True ot
          let ot' = ot </> k
          aiplog ("writing aip document " ++ ot')
          liftIO $ LazyByteString.writeFile ot' r
          liftIO $ close c
          pure ot'

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

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

import Control.Category((.))
import Control.Applicative(pure)
import Control.Lens
import Control.Monad.IO.Class(liftIO)
import Network.HTTP(HandleStream, getAuth, openStream, host, normalizeRequest, defaultNormalizeRequestOptions, close)
import qualified Data.ByteString.Lazy as LazyByteString(ByteString, writeFile)
import Control.Monad.Trans.Except(ExceptT(ExceptT))
import Data.Aviation.Aip.AipCon(AipCon(AipCon))
import Data.Aviation.Aip.Log(aiplog)
import Data.Aviation.Aip.ConnErrorHttp4xx(ConnErrorHttp4xx(IsConnError, Http4xx))
import Data.Aviation.Aip.Href(Href(Href))
import Data.Bool(Bool(True), bool)
import Data.Either(Either(Left, Right))
import Data.Eq(Eq((==)))
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Data.Functor((<$>))
#endif
import Data.Function(($))
import Data.List(isPrefixOf, dropWhile)
import Data.Maybe(Maybe(Just))
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Network.HTTP(HStream, Request, RequestMethod(GET, POST), mkRequest, setRequestBody, simpleHTTP, simpleHTTP_, rspCode, rspBody)
import Network.BufferType(BufferType)
import Network.URI(URI(URI), URIAuth(URIAuth))
import Prelude(Show(show))
import System.Directory(createDirectoryIfMissing)
import System.FilePath(FilePath, splitFileName, isPathSeparator, (</>))
import System.IO(IO)

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
  let s' = bool ("/aip/" <> s) s ("/aip/" `isPrefixOf` s)
  in  mkRequest m (URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) s' z "")

doRequest ::
  HStream a =>
  Request a
  -> AipCon a
doRequest r =
  AipCon . pure .
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
  AipCon . pure .
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
      aiplog ("making request for aip document " <> show q)
      auth <- getAuth q
      aiplog ("making request for aip document with auth " <> show auth)
      c <- liftIO $ openStream (host auth) 80
      r <- doRequest' (normalizeRequest defaultNormalizeRequestOptions q) c
      let (j, k) = splitFileName (hf ^. _Wrapped)
      let ot = d </> dropWhile isPathSeparator j
      aiplog ("output directory for aip document " <> ot)
      do  liftIO $ createDirectoryIfMissing True ot
          let ot' = ot </> k
          aiplog ("writing aip document " <> ot')
          liftIO $ writeFile' ot' r
          liftIO $ close c
          pure ot'

writeFile' ::
  String
  -> LazyByteString.ByteString
  -> IO ()
writeFile' x z =
  let x' = 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
            let repl '?' = '_'
                repl c = c
            in  repl <$> x
#else
            x
#endif
  in  LazyByteString.writeFile x' z

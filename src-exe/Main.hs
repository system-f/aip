{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import System.IO(print)
import Data.Aviation.Aip
import Papa hiding ((.=))
import System.Directory
import System.FilePath
import Network.HTTP
import Data.Aeson

main ::
  IO ()
main =
  do  e <-  runExceptT $ 
              do  x <- getAipRecords'
                  let x' = aipPrefix (_ManyHref . _Wrapped) x
                  mapMOf_ _ManyHref (\k -> liftIO (print k) *> downloadHref k) x'
      print e

basedir ::
  FilePath
basedir =
  "/tmp/def"

cachedir ::
  FilePath
cachedir =
  "/tmp/def-cache"

data ReadCacheAipRecords =
  NotReadable
  | NotDecode
  | NotExists
  | NotConfigured
  | CacheAipRecords AipRecords
  deriving (Eq, Show)

instance SetAipRecords ReadCacheAipRecords where 
instance FoldAipRecords ReadCacheAipRecords where
  _FoldAipRecords =
    _AipRecords
    
instance ManyAipRecords ReadCacheAipRecords where 
  _ManyAipRecords =
    _AipRecords

instance AsAipRecords ReadCacheAipRecords where
  _AipRecords =
    prism'
      CacheAipRecords
      (
        \r -> case r of
          NotReadable ->
            Nothing
          NotDecode ->
            Nothing
          NotExists ->
            Nothing
          NotConfigured ->
            Nothing
          CacheAipRecords x ->
            Just x
      )

filesAipRecords cch r =
  let h :: SHA1
      h = r ^. sha1
      z = cachedir </> hashHex h ".json"
      zz :: IO ReadCacheAipRecords
      zz =
        if isReadOrWriteCache cch
          then
            do  e <- doesFileExist z
                if e
                  then
                    do  p <- getPermissions z
                        if readable p
                          then
                            maybe NotDecode CacheAipRecords <$> decodeFileStrict z
                          else
                            pure NotReadable
                  else
                    pure NotExists
          else
            pure NotConfigured 
  in  undefined

undefined = undefined

aipPrefix ::
  ASetter a b String String
  -> a
  -> b
aipPrefix a =
  let p = "/aip/" in
  a %~ (bool <$> (p ++) <*> id <*> isPrefixOf p)

downloadHref ::
  Href
  -> AipConn () 
downloadHref hf =
      do  
          let q = aipRequestGet hf ""
          auth <- getAuth q
          c <- liftIO $ openStream (host auth) 80
          r <- doRequest' (normalizeRequest defaultNormalizeRequestOptions q) c
          let (j, k) = splitFileName (hf ^. _Wrapped)
          let ot = basedir </> dropWhile isPathSeparator j
          liftIO $
            do  createDirectoryIfMissing True ot
                LazyByteString.writeFile (ot </> k) r
                close c

{- todo

* download function
  * only write cache if succeeds
  * delete getAipRecords that does caching

      f <- downloadFileHTTP url
      a <- f `hasAlreadyCached` cacheDir
      case a of
        Nothing -> -- no cache
          files <- traverseHTTP f -- get all URLs
          downloadAll files outdir
          writeCache f files cacheDir
        Just c ->
          pure c
* All As* requires Many* =>
* logging
* command line args

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}

{-
readCache ::
        FilePath
        -> IO (Maybe AipRecords)
      readCache c =
        if isReadOrWriteCache cch
          then
            do  e <- doesFileExist c
                if e
                  then
                    do  p <- getPermissions c
                        if readable p
                          then
                            decodeFileStrict c :: IO (Maybe (AipRecords))
                          else
                            pure Nothing
                  else
                    pure Nothing
          else
            pure Nothing

      writeCache z rs =
        when (isWriteCache cch) $
          do  createDirectoryIfMissing True (takeDirectory z)
              let conf = defConfig { confIndent = Spaces 2 }
              LazyByteString.writeFile z (encodePretty' conf rs)
      trimSpaces =
          dropWhile isSpace
  in  do  c <- requestAipContents
          let h = hash (UTF8.encode c)
          let z = dir </> hashHex h ".json"
          r <- liftIO $ readCache z
          case r of
            Just v ->
              pure v
            Nothing ->
              let traverseAipDocuments ::

              in  do  let AipDocuments a = foldMap (traverseTree traverseAipDocuments . fromTagTree) (parseTree c)
                      q <- AipDocuments <$> traverse runAipDocument a
                      t <- liftIO getCurrentTime
                      let rs = AipRecords h (AipRecord t q :| [])
                      liftIO $ writeCache z rs
                      pure rs
-}
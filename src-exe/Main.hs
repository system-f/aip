{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import Data.Aviation.Aip
import Papa hiding ((.=))
import System.Directory
import System.FilePath
import Network.HTTP
import Control.Exception hiding (catch)
import Control.Monad.Catch
import System.Posix.Files
import Data.Time

catchIOException :: 
  MonadCatch m =>
  m a ->
  (IOException -> m a)
  -> m a
catchIOException =
  catch

main ::
  IO ()
main =
  do  let basedir = "/tmp/abc"
          sha1dir = "sha1"
      e <- runExceptT $ run ReadWriteCache (basedir </> sha1dir)
      case e of
        Left e' ->
          aiplog ("network or HTTP error " ++ show e')
        Right r ->
          do  aiplog ("end aip " ++ showHash r)
              t <- getCurrentTime
              linkRelative basedir [sha1dir, showHash r] ["date", timeDirectory t]
              linkRelative basedir [sha1dir, showsHash r ".json"] ["date", timeDirectory t ++ ".json"]
              
linkRelative ::
  FilePath
  -> [FilePath]
  -> [FilePath]
  -> IO ()
linkRelative base t fr =
  let fr' = fromMaybe [] (fr ^? _init)
      lk = (".." <$ fr') ++ t
  in  do  createDirectoryIfMissing True (joinPath (base : fr'))
          createSymbolicLink (joinPath lk) (joinPath (base : fr))
          
timeDirectory ::
  UTCTime
  -> FilePath
timeDirectory (UTCTime dy f) =
  let (y, m, d) =
        toGregorian dy
      xx n =
        bool id ('0':) (n < 10) (show n)
  in  concat
        [
          show y
        , "-"
        , xx m
        , "-"
        , xx d
        , "."
        , show (round (f * 1000) :: Integer)
        ]

run ::
  Cache
  -> FilePath
  -> ExceptT ConnErrorHttp4xx IO AipRecords
run cch dir =
  let aipPrefix ::
        ASetter a b String String
        -> a
        -> b
      aipPrefix a =
        let p = "/aip/" in
        a %~ (bool <$> (p ++) <*> id <*> isPrefixOf p)
  in  do  x <- getAipRecords cch dir
          let h = dir </> showHash x
          ee <- liftIO $ doesDirectoryExist h
          let dl = mapMOf_ _ManyHref (\c -> let c' = aipPrefix _Wrapped c in downloadHref h c' *> liftIO (linkDocumentWithoutDate h c')) x
          catchIOException (ee `unless` dl) (\e ->
            do  aiplog ("IO Exception: " ++ show e)
                liftIO $ removeDirectoryRecursive h)
          pure x

test =
  runExceptT $
    do  x <- getAipRecords ReadWriteCache "/tmp/abc"
        let r = toListOf blahAip_Summary_SUP_AIC x
        liftIO $ putStrLn (show r)

blah1 :: Lens' AipRecords (NonEmpty AipRecord)
blah1 = aipRecords1

blah2 :: Traversal' AipRecords AipRecord
blah2 = blah1 . traverse

blah3 :: Traversal' AipRecords AipDocuments2
blah3 = blah2 . aipRecordAipDocuments

blah4 :: Traversal' AipRecords (AipDocument ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa)
blah4 = blah3 . _Wrapped . traverse

blah_Aip_Book :: Traversal' AipRecords Href -- (/aip/)current/aip
blah_Aip_Book = blah4 . _Aip_Book . _3 . _Wrapped . traverse . href

blah_Aip_Charts :: Traversal' AipRecords Href -- /aip/current/aipchart
blah_Aip_Charts = blah4 . _Aip_Charts . _3 . _Wrapped . traverse . _Wrapped . _2 . traverse . href

blah_Aip_DAP :: Traversal' AipRecords Href -- /aip/current/dap
blah_Aip_DAP = blah4 . _Aip_DAP . _3 . _Wrapped . traverse . dapEntries . _Wrapped . traverse . href

blah_Aip_ERSA :: Traversal' AipRecords Href -- /aip/current/ersa
blah_Aip_ERSA = blah4 . _Aip_ERSA . _3 . _ManyHref

blah_Aip_SUP_AIC :: Traversal' AipRecords Href -- /aip/current/sup
blah_Aip_SUP_AIC = blah4 . _Aip_SUP_AIC . _2 . _Wrapped . traverse . href

blahAip_Summary_SUP_AIC :: Traversal' AipRecords Href -- (/aip/)current/SUP_AIP_Summary
blahAip_Summary_SUP_AIC = blah4 . _Aip_Summary_SUP_AIC . _1

undefined = undefined

linkDocumentWithoutDate ::
  FilePath
  -> Href
  -> IO ()
linkDocumentWithoutDate dir x =
  let lk = \c -> mapM_ (\v -> createSymbolicLink (takeFileName c) (dir </> (makeRelative "/" v))) (removeDateFromFilename c)
  in  mapMOf_ _Wrapped lk x
  
downloadHref ::
  FilePath
  -> Href
  -> AipConn () 
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
      liftIO $
        do  createDirectoryIfMissing True ot
            let ot' = ot </> k
            aiplog ("writing aip document " ++ ot')
            LazyByteString.writeFile ot' r
            close c

-- | If a file name ends with "_ddMMyyyy.ext", then remove the "_ddMMyyyy" component of the filename.
--
-- >>> removeDateFromFilename "abc"
-- Nothing
--
-- >>> removeDateFromFilename "abc.pdf"
-- Nothing
--
-- >>> removeDateFromFilename "abc_24MAY2018.pdf"
-- Just "abc.pdf"
removeDateFromFilename ::
  FilePath
  -> Maybe FilePath
removeDateFromFilename p =
  let (q, e) = splitExtensions p
      s =
        let r = reverse q
        in  case r of
              y4:y3:y2:y1:m3:m2:m1:d2:d1:'_':z ->
                let t = and $ zipWith ($) ([(4, isDigit), (3, isUpper), (4, isDigit)] >>= uncurry replicate) [y1,y2,y3,y4,m1,m2,m3,d1,d2]
                in  bool Nothing (Just (reverse z ++ e)) t
              _ ->
                Nothing
  in  s

{- todo

* tar download
* move symlinks to their own directory
* crop pdf files with pdfcrop --margins
* convert pdf to ps with pdftops
* command line args
* tidy up cabal/nix
* write README of general flow
* split out with no dependency on unix

http://classic.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s175.145.html
http://www.airservicesaustralia.com /services/aeronautical-information-and-management-services/electronic-data/

-}

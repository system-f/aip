{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aviation.Aip.ConnErrorHttp4xx
import Data.Aviation.Aip.HttpRequest
import Data.Time
import Papa hiding ((.=))
import System.Directory
import System.FilePath
import System.IO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Zipper
import Codec.Binary.UTF8.String
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Aviation.Aip

traverseAipDocuments ::
  TagTreePos String
  -> AipDocuments1
traverseAipDocuments (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Book")], TagLeaf (TagText tx)]) =
        [Aip_Book href tx ()]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Charts")], TagLeaf (TagText tx)]) =
        [Aip_Charts href tx ()]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Supplements and Aeronautical  Information Circulars (AIC)")]]) =
        [Aip_SUP_AIC href ()]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Departure and Approach Procedures (DAP)")], TagLeaf (TagText tx)]) =
        [Aip_DAP href tx ()]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Designated Airspace Handbook (DAH)")], TagLeaf (TagText tx)]) =
        [Aip_DAH href tx]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "En Route Supplement Australia (ERSA)")], TagLeaf (TagText tx)]) =
        [Aip_ERSA href tx ()]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Precision Approach Terrain Charts and Type A & Type B Obstacle Charts")]]) =
        [Aip_AandB_Charts href]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        let str = "Summary of SUP/AIC Current"
            (p, s) = splitAt (length str) tx
        in  if p == str then
              [Aip_Summary_SUP_AIC href s]
            else
              [] 
      li _ =
        []
  in  AipDocuments (x >>= li)
traverseAipDocuments _ =
  mempty

runX ::
  Cache
  -> FilePath -- basedir
  -> AipConn AipRecords
runX cch dir =
  let readCache ::
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
  in  do  c <- requestAipContents
          let h = hash (Codec.Binary.UTF8.String.encode c)
          let z = dir </> hashHex h ".json"
          r <- liftIO $ readCache z
          case r of
            Just v ->
              pure v
            Nothing ->
              do  let AipDocuments a = foldMap (traverseTree traverseAipDocuments . fromTagTree) (parseTree c)
                  q <- AipDocuments <$> traverse runAipDocument a
                  t <- liftIO getCurrentTime
                  let rs = AipRecords h (AipRecord t q :| [])
                  liftIO $ writeCache z rs
                  pure rs

main ::
  IO ()
main =
  do  x <- runExceptT $ runX ReadWriteCache "/tmp/abc"
      print x

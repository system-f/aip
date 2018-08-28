{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.AipRecords(
  AipRecords(..)
, getAipRecords
) where

import Codec.Binary.UTF8.String as UTF8(encode)
import Data.Aeson(decodeFileStrict)
import Data.Aeson.Encode.Pretty(confIndent, defConfig, Indent(Spaces), encodePretty')
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import Data.Time(getCurrentTime)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.AipDocument(AipDocument(Aip_Book, Aip_Charts, Aip_SUP_AIC, Aip_DAP, Aip_DAH, Aip_ERSA, Aip_AandB_Charts, Aip_Summary_SUP_AIC), runAipDocument)
import Data.Aviation.Aip.AipDate(AipDate(AipDate))
import Data.Aviation.Aip.AipDocuments(AipDocuments1, AipDocuments(AipDocuments))
import Data.Aviation.Aip.AipRecord(AipRecord(AipRecord), ManyAipRecord(_ManyAipRecord))
import Data.Aviation.Aip.Cache(Cache, isReadOrWriteCache, isWriteCache)
import Data.Aviation.Aip.ConnErrorHttp4xx(AipConn)
import Data.Aviation.Aip.Href (Href(Href))
import Data.Aviation.Aip.HttpRequest(requestAipContents)
import Data.Aviation.Aip.SHA1(SHA1, hash, hashHex)
import Control.Monad.IO.Class(liftIO)
import Papa hiding ((.=))
import System.Directory(doesFileExist, getPermissions, readable, createDirectoryIfMissing)
import System.FilePath(takeDirectory, (</>))
import Text.HTML.TagSoup(Tag(TagText))
import Text.HTML.TagSoup.Tree(TagTree(TagBranch, TagLeaf), parseTree)
import Text.HTML.TagSoup.Tree.Zipper(TagTreePos(TagTreePos), fromTagTree, traverseTree)

data AipRecords =
  AipRecords
    SHA1
    (NonEmpty AipRecord)
  deriving (Eq, Show)

instance FromJSON AipRecords where
  parseJSON =
    withObject "AipRecords" $ \v ->
      AipRecords <$>
        v .: "sha1" <*>
        v .: "aiprecords"

instance ToJSON AipRecords where
  toJSON (AipRecords s r) =
    object ["sha1" .= s, "aiprecords" .= r]

instance ManyAipRecord AipRecords where
  _ManyAipRecord f (AipRecords s r) =
    AipRecords s <$> traverse f r

getAipRecords ::
  Cache
  -> FilePath -- basedir
  -> AipConn AipRecords
getAipRecords cch dir =
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
                    TagTreePos String
                    -> AipDocuments1
                  traverseAipDocuments (TagTreePos (TagBranch "ul" [] x) _ _ _) =
                    let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Book")], TagLeaf (TagText tx)]) =
                          [Aip_Book (Href href) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Charts")], TagLeaf (TagText tx)]) =
                          [Aip_Charts (Href href) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Supplements and Aeronautical  Information Circulars (AIC)")]]) =
                          [Aip_SUP_AIC (Href href) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Departure and Approach Procedures (DAP)")], TagLeaf (TagText tx)]) =
                          [Aip_DAP (Href href) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Designated Airspace Handbook (DAH)")], TagLeaf (TagText tx)]) =
                          [Aip_DAH (Href href) (AipDate (trimSpaces tx))]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "En Route Supplement Australia (ERSA)")], TagLeaf (TagText tx)]) =
                          [Aip_ERSA (Href href) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Precision Approach Terrain Charts and Type A & Type B Obstacle Charts")]]) =
                          [Aip_AandB_Charts (Href href)]
                        li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
                          let str = "Summary of SUP/AIC Current"
                              (p, s) = splitAt (length str) tx
                          in  if p == str then
                                [Aip_Summary_SUP_AIC (Href href) (AipDate (trimSpaces s))]
                              else
                                [] 
                        li _ =
                          []
                    in  AipDocuments (x >>= li)
                  traverseAipDocuments _ =
                    mempty
              in  do  let AipDocuments a = foldMap (traverseTree traverseAipDocuments . fromTagTree) (parseTree c)
                      q <- AipDocuments <$> traverse runAipDocument a
                      t <- liftIO getCurrentTime
                      let rs = AipRecords h (AipRecord t q :| [])
                      liftIO $ writeCache z rs
                      pure rs

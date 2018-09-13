{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.AipRecords(
  AipRecords(..)
, AsAipRecords(..)
, FoldAipRecords(..)
, GetAipRecords(..)
, SetAipRecords(..)
, ManyAipRecords(..)
, HasAipRecords(..)
, IsAipRecords(..)  
, getAipRecords
, aipRecords1
, run
) where

import Control.Category((.), id)
import Control.Applicative(pure, (<*>), (<**>))
import Codec.Binary.UTF8.String as UTF8(encode)
import Control.Exception(IOException)
import Control.Lens hiding ((.=))
import Control.Monad((>>=), when, unless)
import Control.Monad.Catch(MonadCatch(catch))
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Except(runExceptT)
import Data.Aeson(decodeFileStrict)
import Data.Aeson.Encode.Pretty(confIndent, defConfig, Indent(Spaces), encodePretty')
import qualified Data.ByteString.Lazy as LazyByteString(writeFile)
import Data.Time(getCurrentTime)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.AipDocument(AipDocument(Aip_Book, Aip_Charts, Aip_SUP_AIC, Aip_DAP, Aip_DAH, Aip_ERSA, Aip_AandB_Charts, Aip_Summary_SUP_AIC), runAipDocument)
import Data.Aviation.Aip.AfterDownload(AfterDownload(AfterDownload), AfterDownloadAipCon)
import Data.Aviation.Aip.AipCon(AipCon)
import Data.Aviation.Aip.SHA1(showHash)
import Data.Aviation.Aip.AipDate(AipDate(AipDate))
import Data.Aviation.Aip.AipOptions(parserAipOptions, aipOptionLog, aipOptionCache, aipOptionOutputDirectory, aipOptionVerbose)
import Data.Aviation.Aip.AipDocuments(AipDocuments1, AipDocuments(AipDocuments))
import Data.Aviation.Aip.AipRecord(AipRecord(AipRecord), ManyAipRecord(_ManyAipRecord), FoldAipRecord, SetAipRecord, FoldAipRecord(_FoldAipRecord))
import Data.Aviation.Aip.Cache(Cache, isReadOrWriteCache, isWriteCache)
import Data.Aviation.Aip.Href(Href(Href), SetHref, FoldHref(_FoldHref), ManyHref(_ManyHref), aipPrefix)
import Data.Aviation.Aip.HttpRequest(requestAipContents, downloadHref)
import Data.Aviation.Aip.Log(aiplog, aiplog')
import Data.Aviation.Aip.SHA1(SHA1, GetSHA1, ManySHA1(_ManySHA1), SetSHA1, HasSHA1(sha1), FoldSHA1(_FoldSHA1), hash, hashHex)
import Data.Bool(Bool(True))
import Data.Char(isSpace)
import Data.Either(Either(Left, Right))
import Data.Eq(Eq((==)))
import Data.Foldable(length, foldMap)
import Data.Function(($))
import Data.Functor(fmap, (<$>))
import Data.List(dropWhile, splitAt)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(Monoid(mempty))
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Options.Applicative(execParser, info, helper, fullDesc, header)
import Prelude(Show(show))
import System.Directory(doesDirectoryExist, doesFileExist, getPermissions, readable, createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.FilePath(takeDirectory, (</>), FilePath)
import System.IO(IO, putStrLn)
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

class ManyAipRecords a => AsAipRecords a where
  _AipRecords ::
    Prism' a AipRecords
  default _AipRecords ::
    IsAipRecords a =>
    Prism' a AipRecords
  _AipRecords =
    _IsAipRecords
    
instance AsAipRecords AipRecords where
  _AipRecords =
    id

class FoldAipRecords a where
  _FoldAipRecords ::
    Fold a AipRecords
    
instance FoldAipRecords AipRecords where
  _FoldAipRecords =
    id

class FoldAipRecords a => GetAipRecords a where
  _GetAipRecords ::
    Getter a AipRecords
  default _GetAipRecords ::
    HasAipRecords a =>
    Getter a AipRecords
  _GetAipRecords =
    aipRecords
    
instance GetAipRecords AipRecords where
  _GetAipRecords =
    id

class SetAipRecords a where
  _SetAipRecords ::
    Setter' a AipRecords
  default _SetAipRecords ::
    ManyAipRecords a =>
    Setter' a AipRecords
  _SetAipRecords =
    _ManyAipRecords

instance SetAipRecords AipRecords where
  _SetAipRecords =
    id

class (FoldAipRecords a, SetAipRecords a) => ManyAipRecords a where
  _ManyAipRecords ::
    Traversal' a AipRecords

instance ManyAipRecords AipRecords where
  _ManyAipRecords =
    id

class (GetAipRecords a, ManyAipRecords a) => HasAipRecords a where
  aipRecords ::
    Lens' a AipRecords
  default aipRecords ::
    IsAipRecords a =>
    Lens' a AipRecords
  aipRecords =
    _IsAipRecords

instance HasAipRecords AipRecords where
  aipRecords =
    id

class (HasAipRecords a, AsAipRecords a) => IsAipRecords a where
  _IsAipRecords ::
    Iso' a AipRecords
    
instance IsAipRecords AipRecords where
  _IsAipRecords =
    id

instance SetAipRecords () where
instance FoldAipRecords () where
  _FoldAipRecords =
    _ManyAipRecords
instance ManyAipRecords () where
  _ManyAipRecords _ x =
    pure x

getAipRecords ::
  Cache
  -> FilePath -- basedir
  -> AipCon AipRecords
getAipRecords cch dir =
  let readCache ::
        FilePath
        -> AipCon (Maybe AipRecords)
      readCache c =
        if isReadOrWriteCache cch
          then
            do  e <- liftIO $ doesFileExist c
                if e
                  then
                    do  p <- liftIO $ getPermissions c
                        if readable p
                          then
                            do  aiplog "reading aip contents cache"
                                liftIO $ decodeFileStrict c :: AipCon (Maybe (AipRecords))
                          else
                            do  aiplog "aip contents cache no read permission"
                                pure Nothing
                  else
                    do  aiplog "aip contents cache not exists"
                        pure Nothing
          else
            do  aiplog "configured for no read aip contents cache"
                pure Nothing

      writeCache z rs =
        when (isWriteCache cch) $
          do  aiplog "writing aip contents cache"
              liftIO $ createDirectoryIfMissing True (takeDirectory z)
              let conf = defConfig { confIndent = Spaces 2 }
              liftIO $ LazyByteString.writeFile z (encodePretty' conf rs)
      trimSpaces =
          dropWhile isSpace
  in  do  c <- requestAipContents          
          let h = hash (UTF8.encode c)
          let h' = hashHex h
          aiplog ("aip contents, sha1: " <> h' "")
          let z = dir </> h' ".json"
          r <- readCache z
          case r of
            Just v ->
              do  aiplog "using and returning aip contents cache"
                  pure v
            Nothing ->
              let traverseAipDocuments ::
                    TagTreePos String
                    -> AipDocuments1
                  traverseAipDocuments (TagTreePos (TagBranch "ul" [] x) _ _ _) =
                    let li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "AIP Book")], TagLeaf (TagText tx)]) =
                          [Aip_Book (Href hf) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "AIP Charts")], TagLeaf (TagText tx)]) =
                          [Aip_Charts (Href hf) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "AIP Supplements and Aeronautical  Information Circulars (AIC)")]]) =
                          [Aip_SUP_AIC (Href hf) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "Departure and Approach Procedures (DAP)")], TagLeaf (TagText tx)]) =
                          [Aip_DAP (Href hf) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "Designated Airspace Handbook (DAH)")], TagLeaf (TagText tx)]) =
                          [Aip_DAH (Href hf) (AipDate (trimSpaces tx))]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "En Route Supplement Australia (ERSA)")], TagLeaf (TagText tx)]) =
                          [Aip_ERSA (Href hf) (AipDate (trimSpaces tx)) ()]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText "Precision Approach Terrain Charts and Type A & Type B Obstacle Charts")]]) =
                          [Aip_AandB_Charts (Href hf)]
                        li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText tx)]]) =
                          let st = "Summary of SUP/AIC Current"
                              (p, s) = splitAt (length st) tx
                          in  if p == st then
                                [Aip_Summary_SUP_AIC (Href hf) (AipDate (trimSpaces s))]
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
                      aiplog ("traverse aip records at time " <> show t)
                      let rs = AipRecords h (AipRecord t q :| [])
                      writeCache z rs
                      pure rs

----

instance FoldAipRecord AipRecords where
  _FoldAipRecord =
    _ManyAipRecord

instance SetAipRecord AipRecords where
  
instance ManyAipRecord AipRecords where
  _ManyAipRecord f (AipRecords s r) =
    AipRecords s <$> traverse f r

instance SetHref AipRecords where
instance FoldHref AipRecords where
  _FoldHref =
    _ManyHref

instance ManyHref AipRecords where
  _ManyHref f (AipRecords s r) =
    AipRecords <$> pure s <*> (traverse . _ManyHref) f r

instance FoldSHA1 AipRecords where
  _FoldSHA1 =
    sha1

instance GetSHA1 AipRecords where
  
instance ManySHA1 AipRecords where
  _ManySHA1 =
    sha1

instance SetSHA1 AipRecords where
  
instance HasSHA1 AipRecords where
  sha1 k (AipRecords s r) =
    fmap (\s' -> AipRecords s' r) (k s)

aipRecords1 ::
  Lens' AipRecords (NonEmpty AipRecord)
aipRecords1 k (AipRecords s r) =
  fmap (\r' -> AipRecords s r') (k r)


run ::
  AfterDownloadAipCon a
  -> IO ()
run k =
  let writeAip ::
        AfterDownloadAipCon a
        -> Cache
        -> FilePath
        -> AipCon AipRecords
      writeAip (AfterDownload w) cch dir =
        let catchIOException :: 
              MonadCatch m =>
              m a ->
              (IOException -> m a)
              -> m a
            catchIOException =
              catch
        in  do  x <- getAipRecords cch dir
                let h = dir </> showHash x
                de <- liftIO $ doesDirectoryExist h
                let dl = mapMOf_ _ManyHref (\c -> downloadHref h c >>= \z -> w z c) (aipPrefix x)
                catchIOException (de `unless` dl) (\e ->
                  do  aiplog ("IO Exception: " <> show e)
                      liftIO $ removeDirectoryRecursive h)
                pure x
      p =
        execParser
          (info (parserAipOptions <**> helper) (
            fullDesc <>
            header "aip 0.1.0 <http://www.airservicesaustralia.com/aip/aip.asp>"
          )
        )
  in  do  opts <- p
          let lg = (opts ^. aipOptionLog)
          e <- runExceptT ((writeAip k (opts ^. aipOptionCache) (opts ^. aipOptionOutputDirectory) ^. _Wrapped) lg)
          case e of
            Left e' ->
              do  when lg (aiplog' ("network or HTTP error " <> show e'))
                  exitWith (ExitFailure 1)
            Right r ->
              when (opts ^. aipOptionVerbose) (putStrLn (show r))

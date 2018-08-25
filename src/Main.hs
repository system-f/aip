{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main(
  main
) where

import Control.Monad((>=>))
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
import Data.Digest.SHA1
import Network.TCP
import Text.StringLike(StringLike)

newtype SHA1 =
  SHA1
    Word160
  deriving (Eq, Show)

instance FromJSON SHA1 where
  parseJSON v =
    (\(b0, b1, b2, b3, b4) -> SHA1 (Word160 b0 b1 b2 b3 b4)) <$> parseJSON v

instance ToJSON SHA1 where
  toJSON (SHA1 (Word160 b0 b1 b2 b3 b4)) =
    toJSON (b0, b1, b2, b3, b4)

data AipRecord =
  AipRecord
    SHA1
    UTCTime
    [FilePath]
  deriving (Eq, Show)

instance FromJSON AipRecord where
  parseJSON =
    withObject "AipRecord" $ \v ->
      AipRecord <$>
        v .: "sha1" <*>
        v .: "utc" <*>
        v .: "files"

instance ToJSON AipRecord where
  toJSON (AipRecord s t p) =
    object ["sha1" .= s, "utc" .= t, "files" .= p]

newtype AipRecords =
  AipRecords
    [AipRecord]
  deriving (Eq, Show)

instance Monoid AipRecords where
  mempty =
    AipRecords mempty
  AipRecords x `mappend` AipRecords y =
    AipRecords (x `mappend` y)

instance Cons AipRecords AipRecords AipRecord AipRecord where
  _Cons =
    prism'
      (\(h, AipRecords t) -> AipRecords (h `cons` t))
      (\(AipRecords x) -> fmap (fmap AipRecords) (uncons x))

instance AsEmpty AipRecords where
  _Empty =
    prism'
      (\() -> AipRecords [])
      (\(AipRecords x) -> case x of
                            [] ->
                              Just ()
                            _:_ ->
                              Nothing)

instance FromJSON AipRecords where
  parseJSON v =
    AipRecords <$> parseJSON v

instance ToJSON AipRecords where
  toJSON (AipRecords x) =
    toJSON x

class ManyAipRecord a where
  _ManyAipRecord ::
    Traversal' a AipRecord

instance ManyAipRecord AipRecord where
  _ManyAipRecord =
    id

instance ManyAipRecord AipRecords where
  _ManyAipRecord f (AipRecords x) =
    AipRecords <$> traverse f x

aiprecords ::
  FilePath
aiprecords =
  "aip-records.json"

runY ::
  String
  -> ExceptT ConnErrorHttp4xx IO (UTCTime, [FilePath])
runY s =
  do  t <- liftIO getCurrentTime
      let a = foldMap (traverseTree traverseAipDocuments . fromTagTree) (parseTree s)
      AipDocuments' tr2 <- runs3 a
      liftIO $ Papa.mapM_ print tr2
      pure (t, ["file", "file2"])

runs3 ::
  AipDocuments' book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO (AipDocuments' ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa)
runs3 (AipDocuments' d) =
  AipDocuments' <$> traverse runAipDocument d

data ListItemLink =
  ListItemLink
    String
    String
  deriving (Eq, Ord, Show)

newtype ListItemLinks =
  ListItemLinks
    [ListItemLink]
  deriving (Eq, Ord, Show)

instance Semigroup ListItemLinks where
  ListItemLinks x <> ListItemLinks y =
    ListItemLinks (x <> y)

instance Monoid ListItemLinks where
  mappend =
    (<>)
  mempty =
    ListItemLinks []

class ManyListItemLink a where
  _ManyListItemLink ::
    Traversal' a ListItemLink

instance ListItemLinks ~ x => Rewrapped ListItemLinks x

instance Wrapped ListItemLinks where
  type Unwrapped ListItemLinks =
    [ListItemLink]
  _Wrapped' =
    iso (\(ListItemLinks x) -> x) ListItemLinks



instance ManyListItemLink ListItemLink where
  _ManyListItemLink =
    id

instance ManyListItemLink ListItemLinks where
  _ManyListItemLink f (ListItemLinks x) =
    ListItemLinks <$> traverse f x

newtype ListItemLinks1 =
  ListItemLinks1
    [NonEmpty ListItemLink]
  deriving (Eq, Ord, Show)

instance Semigroup ListItemLinks1 where
  ListItemLinks1 x <> ListItemLinks1 y =
    ListItemLinks1 (x <> y)

instance Monoid ListItemLinks1 where
  mappend =
    (<>)
  mempty =
    ListItemLinks1 mempty

traverseListItems ::
  (String -> Bool)
  -> TagTreePos String
  -> ListItemLinks
traverseListItems p (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        if p href
          then
            [ListItemLink href tx]
          else
            []
      li _ =
        []
  in  ListItemLinks (x >>= li)
traverseListItems _ _ =
  ListItemLinks []

data Aip_SUP_and_AIC =
  Aip_SUP_and_AIC 
    String
    String
    String
    String
    String
  deriving (Eq, Ord, Show)

newtype Aip_SUP_and_AICs =
  Aip_SUP_and_AICs
    [Aip_SUP_and_AIC]
  deriving (Eq, Ord, Show)


instance Semigroup Aip_SUP_and_AICs where
  Aip_SUP_and_AICs x <> Aip_SUP_and_AICs y =
    Aip_SUP_and_AICs (x <> y)

instance Monoid Aip_SUP_and_AICs where
  mappend =
    (<>)
  mempty =
    Aip_SUP_and_AICs mempty

data ErsaAerodrome =
  ErsaAerodrome
    String
    String
    (Maybe String)
  deriving (Eq, Ord, Show)

newtype ErsaAerodromes =
  ErsaAerodromes
    [ErsaAerodrome]
  deriving (Eq, Ord, Show)

instance Semigroup ErsaAerodromes where
  ErsaAerodromes x <> ErsaAerodromes y =
    ErsaAerodromes (x <> y)

instance Monoid ErsaAerodromes where
  mappend =
    (<>)
  mempty =
    ErsaAerodromes []

data Ersa =
  Ersa
    ListItemLinks
    ErsaAerodromes
  deriving (Eq, Ord, Show)
  
instance Semigroup Ersa where
  Ersa l1 a1 <> Ersa l2 a2 =
    Ersa (l1 <> l2) (a1 <> a2)

instance Monoid Ersa where
  mappend =
    (<>)
  mempty =
    Ersa mempty mempty

traverseAipHtmlRequestGet ::
  (HStream str, Monoid a, Text.StringLike.StringLike str) =>
  (TagTreePos str -> a)
  -> String
  -> ExceptT ConnErrorHttp4xx IO a
traverseAipHtmlRequestGet k u =
  foldMap (traverseTree k . fromTagTree) . parseTree <$> doRequest (aipRequestGet u "")

runX ::
  FilePath -- basedir
  -> ExceptT ConnErrorHttp4xx IO AipRecord
runX dir =
  let aiprecords' =
        dir </> aiprecords
  in  do  c <-  requestAipContents
          let s = SHA1 (hash (Codec.Binary.UTF8.String.encode c))
          e <-  liftIO (doesFileExist aiprecords')
          x <-  liftIO $
                  if e
                    then
                      decodeFileStrict aiprecords' :: IO (Maybe (AipRecords))
                    else
                      pure Nothing
          let w = x >>= findOf _ManyAipRecord (\(AipRecord h _ _) -> h == s)
          case w of
            Nothing ->
              do  (t, p) <- runY c
                  let r = AipRecord s t p
                  liftIO $ encodeFile aiprecords' (r `cons` fromMaybe mempty x)
                  pure r
            Just v ->
              pure v

main ::
  IO ()
main =
  do  sequence_ [createDirectoryIfMissing True "/tmp/abc", writeFile ("/tmp/abc" </> aiprecords) "", removeFile ("/tmp/abc" </> aiprecords)] -- stop cache
      x <- runExceptT $ runX "/tmp/abc"
      print x

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
  in  AipDocuments' (x >>= li)
traverseAipDocuments _ =
  mempty

data AipDocument book charts sup_aic dap ersa =
  Aip_Book String String book
  | Aip_Charts String String charts
  | Aip_SUP_AIC String sup_aic
  | Aip_Summary_SUP_AIC String String
  | Aip_DAP String String dap
  | Aip_DAH String String
  | Aip_ERSA String String ersa
  | Aip_AandB_Charts String
  deriving (Eq, Ord, Show)

newtype AipDocuments' book charts sup_aic dap ersa =
  AipDocuments'
    [AipDocument book charts sup_aic dap ersa]
  deriving (Eq, Ord, Show)

instance Monoid (AipDocuments' book charts sup_aic dap ersa) where
  mempty =
    AipDocuments'
      mempty
  AipDocuments' x `mappend` AipDocuments' y =
    AipDocuments' (x `mappend` y)

type AipDocuments1 =
  AipDocuments' () () () () ()

type AipDocument2 =
  AipDocument ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa

runBook ::
  AipDocument book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO (AipDocument ListItemLinks charts sup_aic dap ersa)
runBook (Aip_Book u t _) =
  Aip_Book u t <$> traverseAipHtmlRequestGet (traverseListItems (isSuffixOf ".pdf")) u
runBook (Aip_Charts u t x) =
  pure (Aip_Charts u t x)
runBook (Aip_SUP_AIC u x) =
  pure (Aip_SUP_AIC u x)
runBook (Aip_Summary_SUP_AIC u x) =
  pure (Aip_Summary_SUP_AIC u x)
runBook (Aip_DAP u t x) =
  pure (Aip_DAP u t x)
runBook (Aip_DAH u x) =
  pure (Aip_DAH u x)
runBook (Aip_ERSA u t x) =
  pure (Aip_ERSA u t x)
runBook (Aip_AandB_Charts x) =
  pure (Aip_AandB_Charts x)

runCharts ::
  AipDocument book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO (AipDocument book ListItemLinks1 sup_aic dap ersa)
runCharts (Aip_Book u t x) =
  pure (Aip_Book u t x)
runCharts (Aip_Charts u t _) =
  do  i <- traverseAipHtmlRequestGet (traverseListItems (const True)) u
      p <- traverse (\l@(ListItemLink u' _) ->
              do  n <- traverseAipHtmlRequestGet (traverseListItems (isSuffixOf ".pdf")) u'
                  pure (l :| n ^. _Wrapped)) (i ^. _Wrapped)
      pure (Aip_Charts u t (ListItemLinks1 p))
runCharts (Aip_SUP_AIC u x) =
  pure (Aip_SUP_AIC u x)
runCharts (Aip_Summary_SUP_AIC u x) =
  pure (Aip_Summary_SUP_AIC u x)
runCharts (Aip_DAP u t x) =
  pure (Aip_DAP u t x)
runCharts (Aip_DAH u x) =
  pure (Aip_DAH u x)
runCharts (Aip_ERSA u t x) =
  pure (Aip_ERSA u t x)
runCharts (Aip_AandB_Charts x) =
  pure (Aip_AandB_Charts x)

runSUP_AIC ::
  AipDocument book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO (AipDocument book charts Aip_SUP_and_AICs dap ersa)
runSUP_AIC (Aip_Book u t x) =
  pure (Aip_Book u t x)
runSUP_AIC (Aip_Charts u t x) =
  pure (Aip_Charts u t x)
runSUP_AIC (Aip_SUP_AIC u _) =
  let traverseAip_SUP_AIC ::
        TagTreePos String
        -> Aip_SUP_and_AICs
      traverseAip_SUP_AIC (TagTreePos (TagBranch "tr" _ (TagLeaf (TagText _) : TagBranch "td" [] [TagLeaf (TagText docnum)] : TagLeaf (TagText _): TagBranch "td" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText title)]] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText pubdate)] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText effdate)] : _)) _ _ _) =
        Aip_SUP_and_AICs [Aip_SUP_and_AIC docnum href title pubdate effdate]
      traverseAip_SUP_AIC _ =
        mempty
  in  Aip_SUP_AIC u <$> traverseAipHtmlRequestGet traverseAip_SUP_AIC u
runSUP_AIC (Aip_Summary_SUP_AIC u x) =
  pure (Aip_Summary_SUP_AIC u x)
runSUP_AIC (Aip_DAP u t x) =
  pure (Aip_DAP u t x)
runSUP_AIC (Aip_DAH u x) =
  pure (Aip_DAH u x)
runSUP_AIC (Aip_ERSA u t x) =
  pure (Aip_ERSA u t x)
runSUP_AIC (Aip_AandB_Charts x) =
  pure (Aip_AandB_Charts x)

data DAPType =
  SpecNotManTOCDAP
  | ChecklistTOCDAP
  | LegendInfoTablesTOCDAP
  | AeroProcChartsTOCDAP
  deriving (Eq, Ord, Show)

data DAPEntry =
  DAPEntry
    String -- href
    String -- date
    String -- amend
  deriving (Eq, Ord, Show)

newtype DAPEntries =
  DAPEntries
    [DAPEntry]
  deriving (Eq, Ord, Show)

instance Semigroup DAPEntries where
  DAPEntries x <> DAPEntries y =
    DAPEntries (x <> y)

instance Monoid DAPEntries where
  mappend =
    (<>)
  mempty =
    DAPEntries mempty

data DAPDoc =
  DAPDoc
    DAPType
      String -- url
      DAPEntries
  deriving (Eq, Ord, Show)

newtype DAPDocs =
  DAPDocs
    [DAPDoc]
  deriving (Eq, Ord, Show)

instance Semigroup DAPDocs where
  DAPDocs x <> DAPDocs y =
    DAPDocs (x <> y)

instance Monoid DAPDocs where
  mappend =
    (<>)
  mempty =
    DAPDocs mempty

undefined = undefined

runDAP ::
  AipDocument book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO (AipDocument book charts sup_aic DAPDocs ersa)
runDAP (Aip_Book u t x) =
  pure (Aip_Book u t x)
runDAP (Aip_Charts u t x) =
  pure (Aip_Charts u t x)
runDAP (Aip_SUP_AIC u x) =
  pure (Aip_SUP_AIC u x)
runDAP (Aip_Summary_SUP_AIC u x) =
  pure (Aip_Summary_SUP_AIC u x)
runDAP (Aip_DAP u t _) =
  let eachDAP :: ExceptT ConnErrorHttp4xx IO DAPDocs
      eachDAP =
        let traverseDAP2' ::
              TagTreePos String
              -> DAPEntries
            traverseDAP2' (TagTreePos (TagBranch "tr" [] [TagLeaf (TagText _),TagLeaf (TagOpen "td" _),TagLeaf (TagText _),TagBranch "td" _ [TagBranch "a" [("href",href)] [TagLeaf (TagText tx)]],TagLeaf (TagText _),TagBranch "td" _ [TagLeaf (TagText date),TagBranch "span" _ [TagLeaf (TagText amend)]],TagLeaf (TagText _)]) _ _ _) =
              DAPEntries [DAPEntry href date amend]
            traverseDAP2' _ =
              mempty
            traverseDAP' ::
              TagTreePos String
              -> [(DAPType, String)]
            traverseDAP' (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefSpecNotManTOC)] [TagLeaf (TagText "Special Notices & Manuscript")]]) _ _ _) =
              [(SpecNotManTOCDAP, hrefSpecNotManTOC)]
            traverseDAP' (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefChecklistTOC)] [TagLeaf (TagText "Checklist")]]) _ _ _) =
              [(ChecklistTOCDAP, hrefChecklistTOC)]
            traverseDAP' (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefLegendInfoTablesTOC)] [TagLeaf (TagText "Legend. Info & Tables")]]) _ _ _) =
              [(LegendInfoTablesTOCDAP, hrefLegendInfoTablesTOC)]
            traverseDAP' (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefAeroProcChartsTOC)] [TagLeaf (TagText "Aerodrome & Procedure Charts")]]) _ _ _) =
              [(AeroProcChartsTOCDAP, hrefAeroProcChartsTOC)]
            traverseDAP' _ =
              []
        in  do  dap1 <- traverseAipHtmlRequestGet traverseDAP' u
                DAPDocs <$> mapM (\(d, u') -> DAPDoc d u' <$> traverseAipHtmlRequestGet traverseDAP2' u') dap1
  in  Aip_DAP u t <$> eachDAP
runDAP (Aip_DAH u x) =
  pure (Aip_DAH u x)
runDAP (Aip_ERSA u t x) =
  pure (Aip_ERSA u t x)
runDAP (Aip_AandB_Charts x) =
  pure (Aip_AandB_Charts x)

runERSA ::
  AipDocument book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO (AipDocument book charts sup_aic dap Ersa)
runERSA (Aip_Book u t x) =
  pure (Aip_Book u t x)
runERSA (Aip_Charts u t x) =
  pure (Aip_Charts u t x)
runERSA (Aip_SUP_AIC u x) =
  pure (Aip_SUP_AIC u x)
runERSA (Aip_Summary_SUP_AIC u x) =
  pure (Aip_Summary_SUP_AIC u x)
runERSA (Aip_DAP u t x) =
  pure (Aip_DAP u t x)
runERSA (Aip_DAH u x) =
  pure (Aip_DAH u x)
runERSA (Aip_ERSA u t _) =
  let traverseErsaAerodromes ::
        TagTreePos String
        -> ErsaAerodromes
      traverseErsaAerodromes (TagTreePos (TagBranch "tr" [] (TagLeaf (TagText _) : TagBranch "td" _ [TagLeaf (TagText aerodrome)] : TagLeaf (TagText _) : TagBranch "td" _ [TagLeaf (TagText _), TagBranch "a" [("href", fac_href)] [TagLeaf (TagText "FAC")], TagLeaf (TagText _)] : r)) _ _ _) =
        ErsaAerodromes [
          ErsaAerodrome
            aerodrome
            fac_href $
            case r of
              TagLeaf (TagText _) : TagBranch "td" _ [TagLeaf (TagText _), TagBranch "a" [("href", rds_href)] [TagLeaf (TagText "RDS")], TagLeaf (TagText _)] : _ : _ ->
                Just rds_href
              _ ->
                Nothing]
      traverseErsaAerodromes _ =
        ErsaAerodromes []
      traverseErsaDocs ::
        TagTreePos String
        -> ListItemLinks
      traverseErsaDocs =
        traverseListItems (isSuffixOf ".pdf")
  in  Aip_ERSA u t <$> traverseAipHtmlRequestGet (Ersa <$> traverseErsaDocs <*> traverseErsaAerodromes) u
runERSA (Aip_AandB_Charts x) =
  pure (Aip_AandB_Charts x)

runAipDocument ::
  AipDocument book charts sup_aic dap ersa
  -> ExceptT ConnErrorHttp4xx IO AipDocument2
runAipDocument =
  runBook >=> runCharts >=> runSUP_AIC >=> runDAP >=> runERSA
  












{-
import Control.Exitcode(ExitcodeT0, ExitcodeT, fromExitCode, runExitcode)
import Control.Monad((>=>))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Aviation.Aip.AipDocuments(distributeAipDocuments)
import Data.Time(UTCTime(utctDay, utctDayTime), TimeOfDay(TimeOfDay), toGregorian, timeToTimeOfDay, getCurrentTime)
import System.Directory(listDirectory, doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath((</>), takeDirectory, splitFileName, takeExtension)
import System.IO(IO, hPutStrLn, stderr)
import System.Process(CreateProcess(cwd), ProcessHandle, createProcess, waitForProcess, proc)
import Papa

createProcessHandle ::
  CreateProcess
  -> IO ProcessHandle
createProcessHandle =
  fmap (view _4) . createProcess

createMakeWaitProcess ::
  CreateProcess
  -> ExitcodeT0 IO
createMakeWaitProcess c =
  fromExitCode $
    do  mapM_ (createDirectoryIfMissing True) (cwd c)
        (createProcessHandle >=> waitForProcess) c

procIn ::
  FilePath -- ^ the working directory
  -> FilePath
  -> [String]
  -> CreateProcess
procIn dir p s =
  (\q -> q { cwd =  Just dir}) -- todo lens
    (proc p s)

exit ::
  ExitcodeT0 IO
  -> IO ()
exit e =
  toExitCode e >>= exitWith

-- belongs in exitcode
toExitCode ::
  Functor f =>
  ExitcodeT f a
  -> f ExitCode
toExitCode e =
  either ExitFailure (const ExitSuccess) <$> runExitcode e

----

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        adir:_ ->
          do  t <- getCurrentTime
              let u = time t ++ "UTC"
                  d = adir </> u
              void (distributeAipDocuments (d </> "aip") (d </> "log"))
              exit $ do   createMakeWaitProcess . linkLatest adir $ u
                          tarDirectories d (d </> "download")
                          m <- lift (pdffiles (d </> "aip"))
                          mapM_ (\(dty, ext, n) -> convert' dty (d </> "aip") ext n) ((,,) <$> [100, 250] <*> ["png"] <*> m)
        _ ->
          hPutStrLn stderr "<aip-output-directory>"

convert' ::
  Int
  -> FilePath
  -> String
  -> FilePath
  -> ExitcodeT0 IO
convert' dty d ext p =
  let ot = takeDirectory d </> "convert" </> p ++ ".density" ++ show dty ++ "." ++ ext
  in  do  lift (createDirectoryIfMissing True (takeDirectory ot))
          createMakeWaitProcess (convert dty d p ot)

convert ::
  Int
  -> FilePath -- in directory
  -> FilePath -- pdf
  -> FilePath -- image
  -> CreateProcess
convert dty d p q =
  procIn d "convert"
    [
      "-density"
    , show dty
    , p
    , q
    ]

pdffiles ::
  FilePath
  -> IO [FilePath]
pdffiles v =
  let pdffiles' ::
        FilePath
        -> FilePath
        -> IO [FilePath]
      pdffiles' q p =
        do  x <- listDirectory (q </> p)
            let x' = ((p </>) <$> x)
            g <- filterM (\f -> (&& takeExtension (q </> f) == ".pdf") <$> doesFileExist (q </> f)) x'
            d <- filterM (\f -> doesDirectoryExist (q </> f)) x'
            e <- mapM (pdffiles' q) d
            pure (g ++ concat e)
  in pdffiles' v ""

directories ::
  FilePath
  -> IO [FilePath]
directories p =
  listDirectory p >>= \ds -> filterM (\d -> doesDirectoryExist (p </> d)) ds

tarDirectories ::
  FilePath
  -> FilePath
  -> ExitcodeT0 IO
tarDirectories d1 d2 =
  let tarDirectories' r s =
        let tarDirectory ::
              FilePath
              -> FilePath
              -> CreateProcess
            tarDirectory d e =
              let (k, g) = splitFileName d
              in  procIn d "tar"
                    [
                      "-C"
                    , k
                    , "-zcvf"
                    , e ++ ".tar.gz"
                    , g
                    ]
            tarDirectory' ::
              FilePath
              -> FilePath
              -> ExitcodeT0 IO
            tarDirectory' d e =
              do  lift (createDirectoryIfMissing True (takeDirectory e))
                  p <- lift (doesFileExist (e ++ ".tar.gz"))
                  p `unless` createMakeWaitProcess (tarDirectory d e)
        in  do  ds <- lift (directories r)
                mapM_ (\d -> 
                  let r' = r </> d
                      s' = s </> d
                  in  do  (r' == s) `unless` tarDirectory' r' s'
                          tarDirectories' r' s'
                      ) ds
  in  tarDirectories' d1 d2

linkLatest ::
  FilePath
  -> String
  -> CreateProcess
linkLatest d t =
  procIn d "ln"
    [
      "-f"
    , "-s"
    , "-n"
    , t
    , "latest"
    ]

time ::
  UTCTime
  -> String
time t =
  let show2 = let s2 [x] = ['0', x]
                  s2 x = x
              in s2 . show
      (y, m, d) = toGregorian (utctDay t)
      TimeOfDay h n s = timeToTimeOfDay (utctDayTime t)
  in concat [show y, show2 m, show2 d, "-", show2 h, show2 n, show2 (floor s)]
-}

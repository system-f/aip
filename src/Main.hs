{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Data.Digest.SHA1

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
      AipDocuments2 tr2 <- runs2 a
      liftIO $ Papa.mapM_ print tr2
      pure (t, ["file", "file2"])

runs2 ::
  AipDocuments
  -> ExceptT ConnErrorHttp4xx IO AipDocuments2
runs2 (AipDocuments d) =
  AipDocuments2 <$> traverse run2 d

traverseAipBooks ::
  TagTreePos String
  -> [(String, String)]
traverseAipBooks (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        if ".pdf" `isSuffixOf` href
          then
            [(href, tx)]
          else
            []
      li _ =
        []
  in  x >>= li
traverseAipBooks _ =
  []

traverseAipCharts1 ::
  TagTreePos String
  -> [(String, String)]
traverseAipCharts1 (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        [(href, tx)]
      li _ =
        []
  in  x >>= li
traverseAipCharts1 _ =
  []

traverseAipCharts2 ::
  TagTreePos String
  -> [(String, String)]
traverseAipCharts2 (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        if ".pdf" `isSuffixOf` href
          then
            [(href, tx)]
          else
            []
      li _ =
        []
  in  x >>= li
traverseAipCharts2 _ =
  []

traverseAip_SUP_AIC ::
  TagTreePos String
  -> [(String, String, String, String, String)]
traverseAip_SUP_AIC (TagTreePos (TagBranch "tr" _ (TagLeaf (TagText _) : TagBranch "td" [] [TagLeaf (TagText docnum)] : TagLeaf (TagText _): TagBranch "td" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText title)]] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText pubdate)] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText effdate)] : _)) _ _ _) =
  [(docnum, href, title, pubdate, effdate)]
traverseAip_SUP_AIC _ =
  []

traverseDAP2 ::
  TagTreePos String
  -> [(String, String)]
traverseDAP2 (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        if ".htm" `isSuffixOf` href
          then
            [(href, tx)]
          else
            []
      li _ =
        []
  in  x >>= li
traverseDAP2 _ =
  []

run2 ::
  AipDocument
  -> ExceptT ConnErrorHttp4xx IO AipDocument2
run2 (Aip_Book u t) =
  do  r <- doRequest (aipRequestGet u "")
      let q = foldMap (traverseTree traverseAipBooks . fromTagTree) (parseTree r)
      pure (Aip_Book2 u t q)
run2 (Aip_Charts u t) =
  do  r <- doRequest (aipRequestGet u "")
      let q = foldMap (traverseTree traverseAipCharts1 . fromTagTree) (parseTree r)
      q' <- traverse (\(u', t') ->  do  r' <- doRequest (aipRequestGet u' "")
                                        let n = foldMap (traverseTree traverseAipCharts2 . fromTagTree) (parseTree r')
                                        pure (u', n, t')) q
      pure (Aip_Charts2 u t q')
run2 (Aip_SUP_AIC u) =
  do  r <- doRequest (aipRequestGet u "") :: ExceptT ConnErrorHttp4xx IO String
      let q = foldMap (traverseTree traverseAip_SUP_AIC . fromTagTree) (parseTree r)
      pure (Aip_SUP_AIC2 u q)
run2 (Aip_Summary_SUP_AIC u t) =
  pure (Aip_Summary_SUP_AIC2 u t)
run2 (Aip_DAP u t) =
  do  r <- doRequest (aipRequestGet u "")
      let q = foldMap (traverseTree traverseDAP2 . fromTagTree) (parseTree r)
      pure (Aip_DAP2 u t q) -- todo
run2 (Aip_DAH u t) =
  pure (Aip_DAH2 u t)
run2 (Aip_ERSA u t) =
  pure (Aip_ERSA2 u t)
run2 (Aip_AandB_Charts u) =
  pure (Aip_AandB_Charts2 u)

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
  -> AipDocuments
traverseAipDocuments (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Book")], TagLeaf (TagText tx)]) =
        [Aip_Book href tx]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Charts")], TagLeaf (TagText tx)]) =
        [Aip_Charts href tx]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "AIP Supplements and Aeronautical  Information Circulars (AIC)")]]) =
        [Aip_SUP_AIC href]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Departure and Approach Procedures (DAP)")], TagLeaf (TagText tx)]) =
        [Aip_DAP href tx]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "Designated Airspace Handbook (DAH)")], TagLeaf (TagText tx)]) =
        [Aip_DAH href tx]
      li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText "En Route Supplement Australia (ERSA)")], TagLeaf (TagText tx)]) =
        [Aip_ERSA href tx]
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

data AipDocument =
  Aip_Book String String
  | Aip_Charts String String
  | Aip_SUP_AIC String
  | Aip_Summary_SUP_AIC String String
  | Aip_DAP String String
  | Aip_DAH String String
  | Aip_ERSA String String
  | Aip_AandB_Charts String
  deriving (Eq, Ord, Show)

newtype AipDocuments =
  AipDocuments
    [AipDocument]
  deriving (Eq, Ord, Show)

instance Monoid AipDocuments where
  mempty =
    AipDocuments
      mempty
  AipDocuments x `mappend` AipDocuments y =
    AipDocuments (x `mappend` y)

data AipDocument2 =
  Aip_Book2 String String [(String, String)]
  | Aip_Charts2 String String [(String, [(String, String)], String)]
  | Aip_SUP_AIC2 String [(String, String, String, String, String)]
  | Aip_Summary_SUP_AIC2 String String
  | Aip_DAP2 String String [(String, String)]
  | Aip_DAH2 String String
  | Aip_ERSA2 String String
  | Aip_AandB_Charts2 String
  deriving (Eq, Ord, Show)

newtype AipDocuments2 =
  AipDocuments2
    [AipDocument2]
  deriving (Eq, Ord, Show)

instance Monoid AipDocuments2 where
  mempty =
    AipDocuments2
      mempty
  AipDocuments2 x `mappend` AipDocuments2 y =
    AipDocuments2 (x `mappend` y)



















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

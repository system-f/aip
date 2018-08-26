{-# LANGUAGE NoImplicitPrelude #-}

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

import Data.Aviation.Aip

aiprecords ::
  FilePath
aiprecords =
  "aip-records.json"

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

traverseAipHtmlRequestGet ::
  (HStream str, Monoid a, Text.StringLike.StringLike str) =>
  (TagTreePos str -> a)
  -> String
  -> ExceptT ConnErrorHttp4xx IO a
traverseAipHtmlRequestGet k u =
  foldMap (traverseTree k . fromTagTree) . parseTree <$> doGetRequest u ""

data Cache =
  UseCache
  | NoCache
  deriving (Eq, Ord, Show)

runX ::
  Cache
  -> FilePath -- basedir
  -> ExceptT ConnErrorHttp4xx IO AipRecord
runX cch dir =
  let aiprecords' =
        dir </> aiprecords
  in  do  c <-  requestAipContents
          let s = SHA1 (hash (Codec.Binary.UTF8.String.encode c))
          e <-  liftIO (doesFileExist aiprecords')
          x <-  liftIO $
                  if e && cch == UseCache
                    then
                      decodeFileStrict aiprecords' :: IO (Maybe (AipRecords))
                    else
                      pure Nothing
          let w = x >>= findOf _ManyAipRecord (\(AipRecord h _ _) -> h == s)
          case w of
            Nothing ->
              do  let AipDocuments a = foldMap (traverseTree traverseAipDocuments . fromTagTree) (parseTree c)
                  tr2 <- AipDocuments <$> traverse runAipDocument a
                  t <- liftIO getCurrentTime
                  let r = AipRecord s t tr2
                  liftIO $ encodeFile aiprecords' (r `cons` fromMaybe mempty x)
                  pure r
            Just v ->
              pure v

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
  let eachDAP ::
        ExceptT ConnErrorHttp4xx IO DAPDocs
      eachDAP =
        let traverseDAP ::
              TagTreePos String
              -> [(DAPType', String)]
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefSpecNotManTOC)] [TagLeaf (TagText "Special Notices & Manuscript")]]) _ _ _) =
              [(SpecNotManTOCDAP, hrefSpecNotManTOC)]
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefChecklistTOC)] [TagLeaf (TagText "Checklist")]]) _ _ _) =
              [(ChecklistTOCDAP, hrefChecklistTOC)]
              
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefLegendInfoTablesTOC)] [TagLeaf (TagText "Legend. Info & Tables")]]) _ _ _) =
              [(LegendInfoTablesTOCDAP, hrefLegendInfoTablesTOC)]
              
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefAeroProcChartsTOC)] [TagLeaf (TagText "Aerodrome & Procedure Charts")]]) _ _ _) =
              [(AeroProcChartsTOCDAP (), hrefAeroProcChartsTOC)]
            traverseDAP _ =
              []
            traverseDAP2 ::
              TagTreePos String
              -> DAPEntries
            traverseDAP2 (TagTreePos (TagBranch "tr" [] [TagLeaf (TagText _),TagLeaf (TagOpen "td" _),TagLeaf (TagText _),TagBranch "td" _ [TagBranch "a" [("href",href)] [TagLeaf (TagText tx)]],TagLeaf (TagText _),TagBranch "td" _ [TagLeaf (TagText date),TagBranch "span" _ [TagLeaf (TagText amend)]],TagLeaf (TagText _)]) _ _ _) =
              DAPEntries [DAPEntry href tx date amend]
            traverseDAP2 _ =
              mempty
            traverseAeroProcChartsTOCDAP ::
              TagTreePos String
              -> [(String, DAPEntries)]
            traverseAeroProcChartsTOCDAP (TagTreePos (TagBranch "h3" _ [TagLeaf (TagText aerodrome)]) _ (TagLeaf (TagText _) : TagBranch "table" _ es : _) _) =
              [(aerodrome, _Wrapped # (fromTagTree <$> es >>= (^. _Wrapped) . traverseTree traverseDAP2))]
            traverseAeroProcChartsTOCDAP _ =
              mempty
        in  do  dap1 <- traverseAipHtmlRequestGet traverseDAP u
                let ts ::
                      (DAPType', String)
                      -> ExceptT ConnErrorHttp4xx IO [DAPDoc]
                    ts (t', u') =
                      let noaerodrome dt =
                            (\x -> [DAPDoc dt u' x]) <$> traverseAipHtmlRequestGet traverseDAP2 u'
                      in  case t' of
                            SpecNotManTOCDAP ->
                              noaerodrome SpecNotManTOCDAP
                            ChecklistTOCDAP ->
                              noaerodrome ChecklistTOCDAP
                            LegendInfoTablesTOCDAP ->
                              noaerodrome LegendInfoTablesTOCDAP
                            AeroProcChartsTOCDAP () ->
                              do  f <- doRequest (aipRequestGet u' "") :: ExceptT ConnErrorHttp4xx IO String
                                  let es ::
                                        TagTree String
                                        -> [(String, DAPEntries)]
                                      es =
                                        traverseTree traverseAeroProcChartsTOCDAP . fromTagTree
                                      docs ::
                                        [DAPDoc]
                                      docs =
                                        parseTree f >>= \x ->
                                        es x >>= \(s', e') ->
                                        pure (DAPDoc (AeroProcChartsTOCDAP s') u' e')
                                  pure docs
                DAPDocs . concat <$> mapM ts dap1
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

main ::
  IO ()
main =
  do  x <- runExceptT $ runX UseCache "/tmp/abc"
      print x

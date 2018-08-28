{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.AipDocument(
  AipDocument(..)
, AipDocument1
, AipDocument2
, runBook
, runCharts
, runSUP_AIC
, runDAP
, runERSA
, runAipDocument
) where

import Control.Monad(fail, (>=>))
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), Value(Object), object, (.=))
import Data.Aviation.Aip.Aip_SUP_and_AIC(Aip_SUP_and_AIC(Aip_SUP_and_AIC))
import Data.Aviation.Aip.Aip_SUP_and_AICs(Aip_SUP_and_AICs(Aip_SUP_and_AICs))
import Data.Aviation.Aip.AipDate(AipDate(AipDate))
import Data.Aviation.Aip.Amendment(Amendment(Amendment))
import Data.Aviation.Aip.ConnErrorHttp4xx(AipConn)
import Data.Aviation.Aip.DAPType(DAPType', DAPType(SpecNotManTOCDAP, ChecklistTOCDAP, LegendInfoTablesTOCDAP, AeroProcChartsTOCDAP))
import Data.Aviation.Aip.DAPEntries(DAPEntries(DAPEntries))
import Data.Aviation.Aip.DAPEntry(DAPEntry(DAPEntry))
import Data.Aviation.Aip.DAPDoc(DAPDoc(DAPDoc))
import Data.Aviation.Aip.DAPDocs(DAPDocs(DAPDocs))
import Data.Aviation.Aip.DocumentNumber(DocumentNumber(DocumentNumber))
import Data.Aviation.Aip.Ersa(Ersa(Ersa))
import Data.Aviation.Aip.Href(Href(Href), dropHrefFile)
import Data.Aviation.Aip.HttpRequest(doGetRequest)
import Data.Aviation.Aip.ListItemLink(ListItemLink(ListItemLink))
import Data.Aviation.Aip.ListItemLinks(ListItemLinks(ListItemLinks))
import Data.Aviation.Aip.ListItemLinks1(ListItemLinks1(ListItemLinks1))
import Data.Aviation.Aip.ErsaAerodrome(ErsaAerodrome(ErsaAerodrome))
import Data.Aviation.Aip.ErsaAerodromes(ErsaAerodromes(ErsaAerodromes))
import Data.Aviation.Aip.Title(Title(Title))
import Data.Aviation.Aip.Txt(Txt(Txt))
import qualified Data.HashMap.Strict as HashMap(toList)
import Network.TCP(HStream)
import Papa hiding ((.=))
import Text.HTML.TagSoup(Tag(TagText, TagOpen))
import Text.HTML.TagSoup.Tree(TagTree(TagBranch, TagLeaf), parseTree)
import Text.HTML.TagSoup.Tree.Zipper(TagTreePos(TagTreePos), fromTagTree, traverseTree)
import Text.StringLike(StringLike)


data AipDocument book charts sup_aic dap ersa =
  Aip_Book Href AipDate book
  | Aip_Charts Href AipDate charts
  | Aip_SUP_AIC Href sup_aic
  | Aip_Summary_SUP_AIC Href String
  | Aip_DAP Href AipDate dap
  | Aip_DAH Href AipDate
  | Aip_ERSA Href AipDate ersa
  | Aip_AandB_Charts Href
  deriving (Eq, Ord, Show)

type AipDocument1 =
  AipDocument () () () () ()

type AipDocument2 =
  AipDocument ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa

instance (FromJSON book, FromJSON charts, FromJSON sup_aic, FromJSON dap, FromJSON ersa) => FromJSON (AipDocument book charts sup_aic dap ersa) where
  parseJSON (Object z) =
    case HashMap.toList z of
      [("Aip_Book", q)] ->
        (\(u, t, x) -> Aip_Book u t x) <$> parseJSON q
      [("Aip_Charts", q)] ->
        (\(u, t, x) -> Aip_Charts u t x) <$> parseJSON q
      [("Aip_SUP_AIC", q)] ->
        (\(u, x) -> Aip_SUP_AIC u x) <$> parseJSON q
      [("Aip_Summary_SUP_AIC", q)] ->
        (\(u, x) -> Aip_Summary_SUP_AIC u x) <$> parseJSON q
      [("Aip_DAP", q)] ->
        (\(u, t, x) -> Aip_DAP u t x) <$> parseJSON q
      [("Aip_DAH", q)] ->
        (\(u, x) -> Aip_DAH u x) <$> parseJSON q
      [("Aip_ERSA", q)] ->
        (\(u, t, x) -> Aip_ERSA u t x) <$> parseJSON q
      [("Aip_AandB_Charts", q)] ->
        Aip_AandB_Charts <$> parseJSON q
      _ ->
        fail "AipDocument"
  parseJSON _ =
    fail "AipDocument"
    
instance (ToJSON book, ToJSON charts, ToJSON sup_aic, ToJSON dap, ToJSON ersa) => ToJSON (AipDocument book charts sup_aic dap ersa) where
  toJSON (Aip_Book u t x) =
    object ["Aip_Book" .= toJSON (u, t, x)]
  toJSON (Aip_Charts u t x) =
    object ["Aip_Charts" .= toJSON (u, t, x)]
  toJSON (Aip_SUP_AIC u x) =
    object ["Aip_SUP_AIC" .= toJSON (u, x)]
  toJSON (Aip_Summary_SUP_AIC u x) =
    object ["Aip_Summary_SUP_AIC" .= toJSON (u, x)]
  toJSON (Aip_DAP u t x) =
    object ["Aip_DAP" .= toJSON (u, t, x)]
  toJSON (Aip_DAH u x) =
    object ["Aip_DAH" .= toJSON (u, x)]
  toJSON (Aip_ERSA u t x) =
    object ["Aip_ERSA" .= toJSON (u, t, x)]
  toJSON (Aip_AandB_Charts q) =
    object ["Aip_AandB_Charts" .= toJSON q]
  
runBook ::
  AipDocument book charts sup_aic dap ersa
  -> AipConn (AipDocument ListItemLinks charts sup_aic dap ersa)
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
  -> AipConn (AipDocument book ListItemLinks1 sup_aic dap ersa)
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
  -> AipConn (AipDocument book charts Aip_SUP_and_AICs dap ersa)
runSUP_AIC (Aip_Book u t x) =
  pure (Aip_Book u t x)
runSUP_AIC (Aip_Charts u t x) =
  pure (Aip_Charts u t x)
runSUP_AIC (Aip_SUP_AIC u _) =
  let traverseAip_SUP_AIC ::
        TagTreePos String
        -> Aip_SUP_and_AICs
      traverseAip_SUP_AIC (TagTreePos (TagBranch "tr" _ (TagLeaf (TagText _) : TagBranch "td" [] [TagLeaf (TagText docnum)] : TagLeaf (TagText _): TagBranch "td" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText title)]] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText pubdate)] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText effdate)] : _)) _ _ _) =
        Aip_SUP_and_AICs [Aip_SUP_and_AIC (DocumentNumber docnum) (Href href) (Title title) (AipDate pubdate) (AipDate effdate)]
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
  -> AipConn (AipDocument book charts sup_aic DAPDocs ersa)
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
        AipConn DAPDocs
      eachDAP =
        let trimSpaces =
              dropWhile isSpace
            traverseDAP ::
              TagTreePos String
              -> [(DAPType', Href)]
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefSpecNotManTOC)] [TagLeaf (TagText "Special Notices & Manuscript")]]) _ _ _) =
              [(SpecNotManTOCDAP, Href hrefSpecNotManTOC)]
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefChecklistTOC)] [TagLeaf (TagText "Checklist")]]) _ _ _) =
              [(ChecklistTOCDAP, Href hrefChecklistTOC)]
              
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefLegendInfoTablesTOC)] [TagLeaf (TagText "Legend. Info & Tables")]]) _ _ _) =
              [(LegendInfoTablesTOCDAP, Href hrefLegendInfoTablesTOC)]
              
            traverseDAP (TagTreePos (TagBranch "li" [] [TagBranch "a" [("href", hrefAeroProcChartsTOC)] [TagLeaf (TagText "Aerodrome & Procedure Charts")]]) _ _ _) =
              [(AeroProcChartsTOCDAP (), Href hrefAeroProcChartsTOC)]
            traverseDAP _ =
              []
            traverseDAP2 ::
              Href
              -> TagTreePos String
              -> DAPEntries
            traverseDAP2 u' (TagTreePos (TagBranch "tr" [] [TagLeaf (TagText _),TagLeaf (TagOpen "td" _),TagLeaf (TagText _),TagBranch "td" _ [TagBranch "a" [("href",href)] [TagLeaf (TagText tx)]],TagLeaf (TagText _),TagBranch "td" _ [TagLeaf (TagText date),TagBranch "span" _ [TagLeaf (TagText amend)]],TagLeaf (TagText _)]) _ _ _) =
              DAPEntries [DAPEntry (dropHrefFile u' ++ Href href) (Txt tx) (AipDate date) (Amendment (trimSpaces amend))]
            traverseDAP2 _ _ =
              mempty
            traverseAeroProcChartsTOCDAP ::
              Href
              -> TagTreePos String
              -> [(String, DAPEntries)]
            traverseAeroProcChartsTOCDAP u' (TagTreePos (TagBranch "h3" _ [TagLeaf (TagText aerodrome)]) _ (TagLeaf (TagText _) : TagBranch "table" _ es : _) _) =
              [(aerodrome, _Wrapped # (fromTagTree <$> es >>= (^. _Wrapped) . traverseTree (traverseDAP2 u')))]
            traverseAeroProcChartsTOCDAP _ _ =
              mempty
        in  do  dap1 <- traverseAipHtmlRequestGet traverseDAP u
                let ts ::
                      (DAPType', Href)
                      -> AipConn [DAPDoc]
                    ts (t', u') =
                      let noaerodrome dt =
                            (\x -> [DAPDoc dt u' x]) <$> traverseAipHtmlRequestGet (traverseDAP2 u') u'
                      in  case t' of
                            SpecNotManTOCDAP ->
                              noaerodrome SpecNotManTOCDAP
                            ChecklistTOCDAP ->
                              noaerodrome ChecklistTOCDAP
                            LegendInfoTablesTOCDAP ->
                              noaerodrome LegendInfoTablesTOCDAP
                            AeroProcChartsTOCDAP () ->
                              do  f <- doGetRequest u' ""
                                  let es ::
                                        TagTree String
                                        -> [(String, DAPEntries)]
                                      es =
                                        traverseTree (traverseAeroProcChartsTOCDAP u') . fromTagTree
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
  -> AipConn (AipDocument book charts sup_aic dap Ersa)
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
            (Href fac_href) $
            case r of
              TagLeaf (TagText _) : TagBranch "td" _ [TagLeaf (TagText _), TagBranch "a" [("href", rds_href)] [TagLeaf (TagText "RDS")], TagLeaf (TagText _)] : _ : _ ->
                Just (Href rds_href)
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
  -> AipConn AipDocument2
runAipDocument =
  runBook >=> runCharts >=> runSUP_AIC >=> runDAP >=> runERSA

traverseListItems ::
  (String -> Bool)
  -> TagTreePos String
  -> ListItemLinks
traverseListItems p (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", href)] [TagLeaf (TagText tx)]]) =
        if p href
          then
            [ListItemLink (Href href) (Txt tx)]
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
  -> Href
  -> AipConn a
traverseAipHtmlRequestGet k (Href u) =
  foldMap (traverseTree k . fromTagTree) . parseTree <$> doGetRequest (Href u) ""

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.AipDocument(
  AipDocument(..)
, AipDocument1
, AipDocument2
, AsAipDocument(..)
, FoldAipDocument(..)
, GetAipDocument(..)
, SetAipDocument(..)
, ManyAipDocument(..)
, HasAipDocument(..)
, IsAipDocument(..)
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
import Data.Aviation.Aip.AipCon(AipCon)
import Data.Aviation.Aip.AipDate(AipDate(AipDate))
import Data.Aviation.Aip.Amendment(Amendment(Amendment))
import Data.Aviation.Aip.DAPType(DAPType', DAPType(SpecNotManTOCDAP, ChecklistTOCDAP, LegendInfoTablesTOCDAP, AeroProcChartsTOCDAP))
import Data.Aviation.Aip.DAPEntries(DAPEntries(DAPEntries))
import Data.Aviation.Aip.DAPEntry(DAPEntry(DAPEntry))
import Data.Aviation.Aip.DAPDoc(DAPDoc(DAPDoc))
import Data.Aviation.Aip.DAPDocs(DAPDocs(DAPDocs))
import Data.Aviation.Aip.DocumentNumber(DocumentNumber(DocumentNumber))
import Data.Aviation.Aip.Ersa(Ersa(Ersa))
import Data.Aviation.Aip.Href(Href(Href), SetHref, FoldHref(_FoldHref), ManyHref(_ManyHref), dropHrefFile)
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
  | Aip_Summary_SUP_AIC Href AipDate
  | Aip_DAP Href AipDate dap
  | Aip_DAH Href AipDate
  | Aip_ERSA Href AipDate ersa
  | Aip_AandB_Charts Href
  deriving (Eq, Ord, Show)

type AipDocument1 =
  AipDocument () () () () ()

type AipDocument2 =
  AipDocument ListItemLinks ListItemLinks1 Aip_SUP_and_AICs DAPDocs Ersa

class ManyAipDocument a => AsAipDocument a where
  _AipDocument ::
    Prism (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')
  default _AipDocument ::
    IsAipDocument a =>
    Prism (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')
  _AipDocument =
    _IsAipDocument
  _Aip_Book ::
    Prism (a book charts sup_aic dap ersa) (a book' charts sup_aic dap ersa) (Href, AipDate, book) (Href, AipDate, book')
  _Aip_Book =
    _AipDocument .
    prism
        (\(u, t, x) -> Aip_Book u t x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Right (u, t, x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_Charts ::
    Prism (a book charts sup_aic dap ersa) (a book charts' sup_aic dap ersa) (Href, AipDate, charts) (Href, AipDate, charts')
  _Aip_Charts =
    _AipDocument .
    prism
        (\(u, t, x) -> Aip_Charts u t x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Right (u, t, x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_SUP_AIC ::
    Prism (a book charts sup_aic dap ersa) (a book charts sup_aic' dap ersa) (Href, sup_aic) (Href, sup_aic')
  _Aip_SUP_AIC =
    _AipDocument .
    prism
        (\(u, x) -> Aip_SUP_AIC u x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Right (u, x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_Summary_SUP_AIC ::
    Prism (a book charts sup_aic dap ersa) (a book charts sup_aic dap ersa) (Href, AipDate) (Href, AipDate)
  _Aip_Summary_SUP_AIC =
    _AipDocument .
    prism
        (\(u, x) -> Aip_Summary_SUP_AIC u x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Right (u, x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_DAP ::
    Prism (a book charts sup_aic dap ersa) (a book charts sup_aic dap' ersa) (Href, AipDate, dap) (Href, AipDate, dap')
  _Aip_DAP =
    _AipDocument .
    prism
        (\(u, t, x) -> Aip_DAP u t x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Right (u, t, x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_DAH ::
    Prism (a book charts sup_aic dap ersa) (a book charts sup_aic dap ersa) (Href, AipDate) (Href, AipDate)
  _Aip_DAH =
    _AipDocument .
    prism
        (\(u, x) -> Aip_DAH u x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Right (u, x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_ERSA ::
    Prism (a book charts sup_aic dap ersa) (a book charts sup_aic dap ersa') (Href, AipDate, ersa) (Href, AipDate, ersa')
  _Aip_ERSA =
    _AipDocument .
    prism
        (\(u, t, x) -> Aip_ERSA u t x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Right (u, t, x)
                  Aip_AandB_Charts x ->
                    Left (Aip_AandB_Charts x))
  _Aip_AandB_Charts ::
    Prism (a book charts sup_aic dap ersa) (a book charts sup_aic dap ersa) Href Href
  _Aip_AandB_Charts =
    _AipDocument .
    prism
        (\x -> Aip_AandB_Charts x)
        (\a ->  case a of
                  Aip_Book u t x ->
                    Left (Aip_Book u t x)
                  Aip_Charts u t x ->
                    Left (Aip_Charts u t x)
                  Aip_SUP_AIC u x ->
                    Left (Aip_SUP_AIC u x)
                  Aip_Summary_SUP_AIC u x ->
                    Left (Aip_Summary_SUP_AIC u x)
                  Aip_DAP u t x ->
                    Left (Aip_DAP u t x)
                  Aip_DAH u x ->
                    Left (Aip_DAH u x)
                  Aip_ERSA u t x ->
                    Left (Aip_ERSA u t x)
                  Aip_AandB_Charts x ->
                    Right x)

instance AsAipDocument AipDocument where
  _AipDocument =
    id

class FoldAipDocument a where
  _FoldAipDocument ::
    Fold (a book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa)
    
instance FoldAipDocument AipDocument where
  _FoldAipDocument =
    id

class FoldAipDocument a => GetAipDocument a where
  _GetAipDocument ::
    Getter (a book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa)
  default _GetAipDocument ::
    HasAipDocument a =>
    Getter (a book charts sup_aic dap ersa) (AipDocument book charts sup_aic dap ersa)
  _GetAipDocument =
    aipDocument
    
instance GetAipDocument AipDocument where
  _GetAipDocument =
    id

class SetAipDocument a where
  _SetAipDocument ::
    Setter (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')
  default _SetAipDocument ::
    ManyAipDocument a =>
    Setter (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')
  _SetAipDocument =
    _ManyAipDocument

instance SetAipDocument AipDocument where
  _SetAipDocument =
    id

class (FoldAipDocument a, SetAipDocument a) => ManyAipDocument a where
  _ManyAipDocument ::
    Traversal (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa')  (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')

instance ManyAipDocument AipDocument where
  _ManyAipDocument =
    id

class (GetAipDocument a, ManyAipDocument a) => HasAipDocument a where
  aipDocument ::
    Lens (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')
  default aipDocument ::
    IsAipDocument a =>
    Lens (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')
  aipDocument =
    _IsAipDocument

instance HasAipDocument AipDocument where
  aipDocument =
    id

class (HasAipDocument a, AsAipDocument a) => IsAipDocument a where
  _IsAipDocument ::
    Iso (a book charts sup_aic dap ersa) (a book' charts' sup_aic' dap' ersa') (AipDocument book charts sup_aic dap ersa) (AipDocument book' charts' sup_aic' dap' ersa')

instance IsAipDocument AipDocument where
  _IsAipDocument =
    id

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
  
instance (ManyHref book, ManyHref charts, ManyHref sup_aic, ManyHref dap, ManyHref ersa) => SetHref (AipDocument book charts sup_aic dap ersa) where
instance (ManyHref book, ManyHref charts, ManyHref sup_aic, ManyHref dap, ManyHref ersa) => FoldHref (AipDocument book charts sup_aic dap ersa) where
  _FoldHref =
    _ManyHref

instance (ManyHref book, ManyHref charts, ManyHref sup_aic, ManyHref dap, ManyHref ersa) => ManyHref (AipDocument book charts sup_aic dap ersa) where
  _ManyHref f (Aip_Book u d b) =
    Aip_Book <$> f u <*> pure d <*> _ManyHref f b
  _ManyHref f (Aip_Charts u d c) =
    Aip_Charts <$> f u <*> pure d <*> _ManyHref f c
  _ManyHref f (Aip_Summary_SUP_AIC u d) =
    Aip_Summary_SUP_AIC <$> f u <*> pure d
  _ManyHref f (Aip_SUP_AIC u c) =
    Aip_SUP_AIC <$> f u <*> _ManyHref f c
  _ManyHref f (Aip_DAP u d c) =
    Aip_DAP <$> f u <*> pure d <*> _ManyHref f c
  _ManyHref _ (Aip_DAH u c) =
    Aip_DAH <$> pure u <*> pure c
  _ManyHref f (Aip_ERSA u d b) =
    Aip_ERSA <$> f u <*> pure d <*> _ManyHref f b
  _ManyHref _ (Aip_AandB_Charts d) =
    Aip_AandB_Charts <$> pure d

runBook ::
  AipDocument book charts sup_aic dap ersa
  -> AipCon (AipDocument ListItemLinks charts sup_aic dap ersa)
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
  -> AipCon (AipDocument book ListItemLinks1 sup_aic dap ersa)
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
  -> AipCon (AipDocument book charts Aip_SUP_and_AICs dap ersa)
runSUP_AIC (Aip_Book u t x) =
  pure (Aip_Book u t x)
runSUP_AIC (Aip_Charts u t x) =
  pure (Aip_Charts u t x)
runSUP_AIC (Aip_SUP_AIC u _) =
  let traverseAip_SUP_AIC ::
        TagTreePos String
        -> Aip_SUP_and_AICs
      traverseAip_SUP_AIC (TagTreePos (TagBranch "tr" _ (TagLeaf (TagText _) : TagBranch "td" [] [TagLeaf (TagText docnum)] : TagLeaf (TagText _): TagBranch "td" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText title)]] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText pubdate)] : TagLeaf (TagText _) : TagBranch "td" [("align","center")] [TagLeaf (TagText effdate)] : _)) _ _ _) =
        Aip_SUP_and_AICs [Aip_SUP_and_AIC (DocumentNumber docnum) (Href hf) (Title title) (AipDate pubdate) (AipDate effdate)]
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
  -> AipCon (AipDocument book charts sup_aic DAPDocs ersa)
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
        AipCon DAPDocs
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
            traverseDAP2 u' (TagTreePos (TagBranch "tr" [] [TagLeaf (TagText _),TagLeaf (TagOpen "td" _),TagLeaf (TagText _),TagBranch "td" _ [TagBranch "a" [("href",hf)] [TagLeaf (TagText tx)]],TagLeaf (TagText _),TagBranch "td" _ [TagLeaf (TagText date),TagBranch "span" _ [TagLeaf (TagText amend)]],TagLeaf (TagText _)]) _ _ _) =
              DAPEntries [DAPEntry (dropHrefFile u' ++ Href hf) (Txt tx) (AipDate date) (Amendment (trimSpaces amend))]
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
                      -> AipCon [DAPDoc]
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
  -> AipCon (AipDocument book charts sup_aic dap Ersa)
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
      traverseErsaCompletes ::
        TagTreePos String
        -> [Href]
      traverseErsaCompletes (TagTreePos (TagBranch "td" _ [TagLeaf (TagText _),TagBranch "a" [("href", u')] [TagLeaf (TagText "ERSA Complete")],TagLeaf (TagText _)]) _ _ _) =
        [Href u' ]
      traverseErsaCompletes _ =
        []
  in  Aip_ERSA u t <$> traverseAipHtmlRequestGet (Ersa <$> traverseErsaDocs <*> traverseErsaAerodromes <*> traverseErsaCompletes) u
runERSA (Aip_AandB_Charts x) =
  pure (Aip_AandB_Charts x)

runAipDocument ::
  AipDocument book charts sup_aic dap ersa
  -> AipCon AipDocument2
runAipDocument =
  runBook >=> runCharts >=> runSUP_AIC >=> runDAP >=> runERSA

traverseListItems ::
  (String -> Bool)
  -> TagTreePos String
  -> ListItemLinks
traverseListItems p (TagTreePos (TagBranch "ul" [] x) _ _ _) =
  let li (TagBranch "li" [] [TagBranch "a" [("href", hf)] [TagLeaf (TagText tx)]]) =
        if p hf
          then
            [ListItemLink (Href hf) (Txt tx)]
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
  -> AipCon a
traverseAipHtmlRequestGet k (Href u) =
  foldMap (traverseTree k . fromTagTree) . parseTree <$> doGetRequest (Href u) ""

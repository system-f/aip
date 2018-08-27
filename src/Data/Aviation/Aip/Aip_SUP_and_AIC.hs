{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.Aip_SUP_and_AIC(
  Aip_SUP_and_AIC(..)
) where

import Data.Aviation.Aip.AipDate(AipDate)
import Data.Aviation.Aip.DocumentNumber(DocumentNumber)
import Data.Aviation.Aip.Href(Href)
import Data.Aviation.Aip.Title(Title)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Papa hiding ((.=))

data Aip_SUP_and_AIC =
  Aip_SUP_and_AIC 
    DocumentNumber
    Href
    Title
    AipDate
    AipDate
  deriving (Eq, Ord, Show)

instance FromJSON Aip_SUP_and_AIC where
  parseJSON =
    withObject "Aip_SUP_and_AIC" $ \v ->
      Aip_SUP_and_AIC <$>
        v .: "docnum" <*>
        v .: "href" <*>
        v .: "title" <*>
        v .: "pubdate" <*>
        v .: "effdate"

instance ToJSON Aip_SUP_and_AIC where
  toJSON (Aip_SUP_and_AIC docnum href title pubdate effdate) =
    object ["docnum" .= docnum, "href" .= href, "title" .= title, "pubdate" .= pubdate, "effdate" .= effdate]

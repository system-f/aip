{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.ListItemLink(
  ListItemLink(..)
, ManyListItemLink(..)
) where

import Data.Aviation.Aip.Href(Href)
import Data.Aviation.Aip.Txt(Txt)
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Papa hiding ((.=))

data ListItemLink =
  ListItemLink
    Href
    Txt
  deriving (Eq, Ord, Show)

class ManyListItemLink a where
  _ManyListItemLink ::
    Traversal' a ListItemLink

instance ManyListItemLink ListItemLink where
  _ManyListItemLink =
    id

instance FromJSON ListItemLink where
  parseJSON =
    withObject "ListItemLink" $ \v ->
      ListItemLink <$>
        v .: "href" <*>
        v .: "txt" 

instance ToJSON ListItemLink where
  toJSON (ListItemLink u t) =
    object ["href" .= u, "txt" .= t]

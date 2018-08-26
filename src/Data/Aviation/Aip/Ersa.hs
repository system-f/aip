{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Aip.Ersa(
  Ersa(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withObject, object, (.:), (.=))
import Data.Aviation.Aip.ListItemLinks(ListItemLinks)
import Data.Aviation.Aip.ErsaAerodromes(ErsaAerodromes)
import Papa hiding ((.=))

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

instance FromJSON Ersa where
  parseJSON =
    withObject "Ersa" $ \v ->
      Ersa <$>
        v .: "links" <*>
        v .: "aerodromes"

instance ToJSON Ersa where
  toJSON (Ersa links aerodromes) =
    object ["links" .= links, "aerodromes" .= aerodromes]

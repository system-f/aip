{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.Aip_SUP_and_AIC(
  Aip_SUP_and_AIC(..)
, AsAip_SUP_and_AIC(..)
, FoldAip_SUP_and_AIC(..)
, GetAip_SUP_and_AIC(..)
, SetAip_SUP_and_AIC(..)
, ManyAip_SUP_and_AIC(..)
, HasAip_SUP_and_AIC(..)
, IsAip_SUP_and_AIC(..)  
) where

import Data.Aviation.Aip.AipDate(AipDate)
import Data.Aviation.Aip.DocumentNumber(DocumentNumber)
import Data.Aviation.Aip.Href(Href, SetHref, FoldHref(_FoldHref), ManyHref(_ManyHref), GetHref, HasHref(href))
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
  toJSON (Aip_SUP_and_AIC docnum u title pubdate effdate) =
    object ["docnum" .= docnum, "href" .= u, "title" .= title, "pubdate" .= pubdate, "effdate" .= effdate]

class AsAip_SUP_and_AIC a where
  _Aip_SUP_and_AIC ::
    Prism' a Aip_SUP_and_AIC
  default _Aip_SUP_and_AIC ::
    IsAip_SUP_and_AIC a =>
    Prism' a Aip_SUP_and_AIC
  _Aip_SUP_and_AIC =
    _IsAip_SUP_and_AIC
    
instance AsAip_SUP_and_AIC Aip_SUP_and_AIC where
  _Aip_SUP_and_AIC =
    id

class FoldAip_SUP_and_AIC a where
  _FoldAip_SUP_and_AIC ::
    Fold a Aip_SUP_and_AIC
    
instance FoldAip_SUP_and_AIC Aip_SUP_and_AIC where
  _FoldAip_SUP_and_AIC =
    id

class FoldAip_SUP_and_AIC a => GetAip_SUP_and_AIC a where
  _GetAip_SUP_and_AIC ::
    Getter a Aip_SUP_and_AIC
  default _GetAip_SUP_and_AIC ::
    HasAip_SUP_and_AIC a =>
    Getter a Aip_SUP_and_AIC
  _GetAip_SUP_and_AIC =
    aip_SUP_and_AIC
    
instance GetAip_SUP_and_AIC Aip_SUP_and_AIC where
  _GetAip_SUP_and_AIC =
    id

class SetAip_SUP_and_AIC a where
  _SetAip_SUP_and_AIC ::
    Setter' a Aip_SUP_and_AIC
  default _SetAip_SUP_and_AIC ::
    ManyAip_SUP_and_AIC a =>
    Setter' a Aip_SUP_and_AIC
  _SetAip_SUP_and_AIC =
    _ManyAip_SUP_and_AIC

instance SetAip_SUP_and_AIC Aip_SUP_and_AIC where
  _SetAip_SUP_and_AIC =
    id

class (FoldAip_SUP_and_AIC a, SetAip_SUP_and_AIC a) => ManyAip_SUP_and_AIC a where
  _ManyAip_SUP_and_AIC ::
    Traversal' a Aip_SUP_and_AIC

instance ManyAip_SUP_and_AIC Aip_SUP_and_AIC where
  _ManyAip_SUP_and_AIC =
    id

class (GetAip_SUP_and_AIC a, ManyAip_SUP_and_AIC a) => HasAip_SUP_and_AIC a where
  aip_SUP_and_AIC ::
    Lens' a Aip_SUP_and_AIC
  default aip_SUP_and_AIC ::
    IsAip_SUP_and_AIC a =>
    Lens' a Aip_SUP_and_AIC
  aip_SUP_and_AIC =
    _IsAip_SUP_and_AIC

instance HasAip_SUP_and_AIC Aip_SUP_and_AIC where
  aip_SUP_and_AIC =
    id

class (HasAip_SUP_and_AIC a, AsAip_SUP_and_AIC a) => IsAip_SUP_and_AIC a where
  _IsAip_SUP_and_AIC ::
    Iso' a Aip_SUP_and_AIC
    
instance IsAip_SUP_and_AIC Aip_SUP_and_AIC where
  _IsAip_SUP_and_AIC =
    id

instance SetAip_SUP_and_AIC () where
instance FoldAip_SUP_and_AIC () where
  _FoldAip_SUP_and_AIC =
    _ManyAip_SUP_and_AIC
instance ManyAip_SUP_and_AIC () where
  _ManyAip_SUP_and_AIC _ x =
    pure x

----

instance SetHref Aip_SUP_and_AIC where
instance FoldHref Aip_SUP_and_AIC where
  _FoldHref =
    _ManyHref

instance ManyHref Aip_SUP_and_AIC where
  _ManyHref f (Aip_SUP_and_AIC docnum u title pubdate effdate) =
    Aip_SUP_and_AIC <$> pure docnum <*> f u <*> pure title <*> pure pubdate <*> pure effdate

instance GetHref Aip_SUP_and_AIC where
instance HasHref Aip_SUP_and_AIC where
  href f (Aip_SUP_and_AIC docnum u title pubdate effdate) =
    fmap (\u' -> Aip_SUP_and_AIC docnum u' title pubdate effdate) (f u)

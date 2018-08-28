{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.DocumentNumber(
  DocumentNumber(..)
, AsDocumentNumber(..)
, FoldDocumentNumber(..)
, GetDocumentNumber(..)
, SetDocumentNumber(..)
, ManyDocumentNumber(..)
, HasDocumentNumber(..)
, IsDocumentNumber(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype DocumentNumber =
  DocumentNumber
    String
  deriving (Eq, Ord, Show)

instance FromJSON DocumentNumber where
  parseJSON v =
    DocumentNumber <$> parseJSON v

instance ToJSON DocumentNumber where
  toJSON (DocumentNumber x) =
    toJSON x

instance Semigroup DocumentNumber where
  DocumentNumber x <> DocumentNumber y =
    DocumentNumber (x <> y)

instance Monoid DocumentNumber where
  mappend =
    (<>)
  mempty =
    DocumentNumber mempty

instance Wrapped DocumentNumber where
  type Unwrapped DocumentNumber = String
  _Wrapped' =
    iso
      (\(DocumentNumber x) -> x)
      DocumentNumber

instance DocumentNumber ~ a =>
  Rewrapped DocumentNumber a

class AsDocumentNumber a where
  _DocumentNumber ::
    Prism' a DocumentNumber
    
instance AsDocumentNumber DocumentNumber where
  _DocumentNumber =
    id
  
instance AsDocumentNumber String where
  _DocumentNumber =
    from _Wrapped

class FoldDocumentNumber a where
  _FoldDocumentNumber ::
    Fold a DocumentNumber
    
instance FoldDocumentNumber DocumentNumber where
  _FoldDocumentNumber =
    id

instance FoldDocumentNumber String where
  _FoldDocumentNumber =
    from _Wrapped

class FoldDocumentNumber a => GetDocumentNumber a where
  _GetDocumentNumber ::
    Getter a DocumentNumber
    
instance GetDocumentNumber DocumentNumber where
  _GetDocumentNumber =
    id

instance GetDocumentNumber String where
  _GetDocumentNumber =
    from _Wrapped

class SetDocumentNumber a where
  _SetDocumentNumber ::
    Setter' a DocumentNumber
    
instance SetDocumentNumber DocumentNumber where
  _SetDocumentNumber =
    id

instance SetDocumentNumber String where
  _SetDocumentNumber =
    from _Wrapped

class (FoldDocumentNumber a, SetDocumentNumber a) => ManyDocumentNumber a where
  _ManyDocumentNumber ::
    Traversal' a DocumentNumber

instance ManyDocumentNumber DocumentNumber where
  _ManyDocumentNumber =
    id

instance ManyDocumentNumber String where
  _ManyDocumentNumber =
    from _Wrapped

class (GetDocumentNumber a, ManyDocumentNumber a) => HasDocumentNumber a where
  documentNumber ::
    Lens' a DocumentNumber

instance HasDocumentNumber DocumentNumber where
  documentNumber =
    id

instance HasDocumentNumber String where
  documentNumber =
    from _Wrapped

class (HasDocumentNumber a, AsDocumentNumber a) => IsDocumentNumber a where
  _IsDocumentNumber ::
    Iso' a DocumentNumber
    
instance IsDocumentNumber DocumentNumber where
  _IsDocumentNumber =
    id

instance IsDocumentNumber String where
  _IsDocumentNumber =
    from _Wrapped

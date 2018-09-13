{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

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

import Control.Category((.), id)
import Control.Applicative(pure)
import Control.Lens
import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Int(Int)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Prelude(Show)

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

instance Cons DocumentNumber DocumentNumber Char Char where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc DocumentNumber DocumentNumber Char Char where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each DocumentNumber DocumentNumber Char Char where
  each =
    _Wrapped . each

instance Reversing DocumentNumber where
  reversing =
    _Wrapped %~ reversing

instance Plated DocumentNumber where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue DocumentNumber = Char
type instance Index DocumentNumber = Int
instance Ixed DocumentNumber where
  ix i =
    _Wrapped . ix i

instance Wrapped DocumentNumber where
  type Unwrapped DocumentNumber = String
  _Wrapped' =
    iso
      (\(DocumentNumber x) -> x)
      DocumentNumber

instance DocumentNumber ~ a =>
  Rewrapped DocumentNumber a

class ManyDocumentNumber a => AsDocumentNumber a where
  _DocumentNumber ::
    Prism' a DocumentNumber
  default _DocumentNumber ::
    IsDocumentNumber a =>
    Prism' a DocumentNumber
  _DocumentNumber =
    _IsDocumentNumber
    
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
  default _GetDocumentNumber ::
    HasDocumentNumber a =>
    Getter a DocumentNumber
  _GetDocumentNumber =
    documentNumber
    
instance GetDocumentNumber DocumentNumber where
  _GetDocumentNumber =
    id

instance GetDocumentNumber String where
  _GetDocumentNumber =
    from _Wrapped

class SetDocumentNumber a where
  _SetDocumentNumber ::
    Setter' a DocumentNumber
  default _SetDocumentNumber ::
    ManyDocumentNumber a =>
    Setter' a DocumentNumber
  _SetDocumentNumber =
    _ManyDocumentNumber
    
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
  default documentNumber ::
    IsDocumentNumber a =>
    Lens' a DocumentNumber
  documentNumber =
    _IsDocumentNumber

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

instance SetDocumentNumber () where
instance FoldDocumentNumber () where
  _FoldDocumentNumber =
    _ManyDocumentNumber
instance ManyDocumentNumber () where
  _ManyDocumentNumber _ x =
    pure x

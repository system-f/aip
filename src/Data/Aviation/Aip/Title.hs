{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Aip.Title(
  Title(..)
, AsTitle(..)
, FoldTitle(..)
, GetTitle(..)
, SetTitle(..)
, ManyTitle(..)
, HasTitle(..)
, IsTitle(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype Title =
  Title
    String
  deriving (Eq, Ord, Show)

instance FromJSON Title where
  parseJSON v =
    Title <$> parseJSON v

instance ToJSON Title where
  toJSON (Title x) =
    toJSON x

instance Semigroup Title where
  Title x <> Title y =
    Title (x <> y)

instance Monoid Title where
  mappend =
    (<>)
  mempty =
    Title mempty

instance Cons Title Title Char Char where
  _Cons =
    _Wrapped . _Cons . seconding (from _Wrapped)

instance Snoc Title Title Char Char where
  _Snoc =
    _Wrapped . _Snoc . firsting (from _Wrapped)

instance Each Title Title Char Char where
  each =
    _Wrapped . each

instance Reversing Title where
  reversing =
    _Wrapped %~ reversing

instance Plated Title where
  plate =
    _Wrapped . plate . from _Wrapped

type instance IxValue Title = Char
type instance Index Title = Int
instance Ixed Title where
  ix i =
    _Wrapped . ix i

instance Wrapped Title where
  type Unwrapped Title = String
  _Wrapped' =
    iso
      (\(Title x) -> x)
      Title

instance Title ~ a =>
  Rewrapped Title a

class ManyTitle a => AsTitle a where
  _Title ::
    Prism' a Title
  default _Title ::
    IsTitle a =>
    Prism' a Title
  _Title =
    _IsTitle
    
instance AsTitle Title where
  _Title =
    id
  
instance AsTitle String where
  _Title =
    from _Wrapped

class FoldTitle a where
  _FoldTitle ::
    Fold a Title
    
instance FoldTitle Title where
  _FoldTitle =
    id

instance FoldTitle String where
  _FoldTitle =
    from _Wrapped

class FoldTitle a => GetTitle a where
  _GetTitle ::
    Getter a Title
  default _GetTitle ::
    HasTitle a =>
    Getter a Title
  _GetTitle =
    title
    
instance GetTitle Title where
  _GetTitle =
    id

instance GetTitle String where
  _GetTitle =
    from _Wrapped

class SetTitle a where
  _SetTitle ::
    Setter' a Title
  default _SetTitle ::
    ManyTitle a =>
    Setter' a Title
  _SetTitle =
    _ManyTitle
    
instance SetTitle Title where
  _SetTitle =
    id

instance SetTitle String where
  _SetTitle =
    from _Wrapped

class (FoldTitle a, SetTitle a) => ManyTitle a where
  _ManyTitle ::
    Traversal' a Title

instance ManyTitle Title where
  _ManyTitle =
    id

instance ManyTitle String where
  _ManyTitle =
    from _Wrapped

class (GetTitle a, ManyTitle a) => HasTitle a where
  title ::
    Lens' a Title
  default title ::
    IsTitle a =>
    Lens' a Title
  title =
    _IsTitle

instance HasTitle Title where
  title =
    id

instance HasTitle String where
  title =
    from _Wrapped

class (HasTitle a, AsTitle a) => IsTitle a where
  _IsTitle ::
    Iso' a Title
    
instance IsTitle Title where
  _IsTitle =
    id

instance IsTitle String where
  _IsTitle =
    from _Wrapped

instance SetTitle () where
instance FoldTitle () where
  _FoldTitle =
    _ManyTitle
instance ManyTitle () where
  _ManyTitle _ x =
    pure x

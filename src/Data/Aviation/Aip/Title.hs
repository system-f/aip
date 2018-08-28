{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

instance Wrapped Title where
  type Unwrapped Title = String
  _Wrapped' =
    iso
      (\(Title x) -> x)
      Title

instance Title ~ a =>
  Rewrapped Title a

class AsTitle a where
  _Title ::
    Prism' a Title
    
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
    
instance GetTitle Title where
  _GetTitle =
    id

instance GetTitle String where
  _GetTitle =
    from _Wrapped

class SetTitle a where
  _SetTitle ::
    Setter' a Title
    
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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Title(
  Title(..)
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

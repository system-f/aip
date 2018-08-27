{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Amendment(
  Amendment(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype Amendment =
  Amendment
    String
  deriving (Eq, Ord, Show)

instance FromJSON Amendment where
  parseJSON v =
    Amendment <$> parseJSON v

instance ToJSON Amendment where
  toJSON (Amendment x) =
    toJSON x

instance Semigroup Amendment where
  Amendment x <> Amendment y =
    Amendment (x <> y)

instance Monoid Amendment where
  mappend =
    (<>)
  mempty =
    Amendment mempty

instance Wrapped Amendment where
  type Unwrapped Amendment = String
  _Wrapped' =
    iso
      (\(Amendment x) -> x)
      Amendment

instance Amendment ~ a =>
  Rewrapped Amendment a

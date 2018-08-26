{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.Aip_SUP_and_AICs(
  Aip_SUP_and_AICs(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.Aip_SUP_and_AIC(Aip_SUP_and_AIC)
import Papa

newtype Aip_SUP_and_AICs =
  Aip_SUP_and_AICs
    [Aip_SUP_and_AIC]
  deriving (Eq, Ord, Show)

instance Semigroup Aip_SUP_and_AICs where
  Aip_SUP_and_AICs x <> Aip_SUP_and_AICs y =
    Aip_SUP_and_AICs (x <> y)

instance Monoid Aip_SUP_and_AICs where
  mappend =
    (<>)
  mempty =
    Aip_SUP_and_AICs mempty

instance FromJSON Aip_SUP_and_AICs where
  parseJSON =
    withArray "Aip_SUP_and_AICs" $ \v ->
      Aip_SUP_and_AICs <$> traverse parseJSON (toList v)

instance ToJSON Aip_SUP_and_AICs where
  toJSON (Aip_SUP_and_AICs x) =
    toJSON x

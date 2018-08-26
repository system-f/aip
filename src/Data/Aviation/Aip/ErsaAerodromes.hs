{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Aip.ErsaAerodromes(
  ErsaAerodromes(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON), withArray)
import Data.Aviation.Aip.ErsaAerodrome(ErsaAerodrome)
import Papa

newtype ErsaAerodromes =
  ErsaAerodromes
    [ErsaAerodrome]
  deriving (Eq, Ord, Show)

instance Semigroup ErsaAerodromes where
  ErsaAerodromes x <> ErsaAerodromes y =
    ErsaAerodromes (x <> y)

instance Monoid ErsaAerodromes where
  mappend =
    (<>)
  mempty =
    ErsaAerodromes []

instance FromJSON ErsaAerodromes where
  parseJSON =
    withArray "ErsaAerodromes" $ \v ->
      ErsaAerodromes <$> traverse parseJSON (toList v)

instance ToJSON ErsaAerodromes where
  toJSON (ErsaAerodromes x) =
    toJSON x

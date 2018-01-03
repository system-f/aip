{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.Ersa(
  Ersa(..)
, HasErsa(..)
) where

import Data.Aviation.Aip.AipHref(AipHref)
import Data.Aviation.Aip.AipDate(AipDate)
import Papa

data Ersa =
  Ersa {
    _ersaHref ::
      AipHref
  , _ersaDate ::
      AipDate
  } deriving (Eq, Ord, Show)

makeClassy ''Ersa

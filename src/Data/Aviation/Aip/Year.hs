{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.Year(
  Year(..)
, parseYear
, HasYear(..)
) where

import Data.Digit(Digit, parseDecimal)
import Text.Parser.Char(CharParsing)
import Papa

data Year =
  Year {
    _year1 ::
      Digit
  , _year2 ::
      Digit
  , _year3 ::
      Digit
  , _year4 ::
      Digit
  }
  deriving (Eq, Ord, Show)

parseYear ::
  (CharParsing p, Monad p) =>
  p Year
parseYear =
  Year <$> parseDecimal <*> parseDecimal <*> parseDecimal <*> parseDecimal

makeClassy ''Year

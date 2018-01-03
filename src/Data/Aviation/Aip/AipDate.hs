{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.AipDate(
  AipDate(..)
, parseAipDate
, uriAipDate
, HasAipDate(..)
) where

import Data.Aviation.Aip.Day(Day(Day), HasDay(day), parseDay)
import Data.Aviation.Aip.Month(Month, HasMonth(month), parseMonth)
import Data.Aviation.Aip.Year(Year(Year), HasYear(year), parseYear)
import Text.Parser.Char(CharParsing, char)
import Papa

data AipDate =
  AipDate {
    _aipday ::
      Day
  , _aipmonth ::
      Month
  , _aipyear ::
      Year
  } deriving (Eq, Ord, Show)

makeClassy ''AipDate

parseAipDate ::
  (CharParsing p, Monad p) =>
  p AipDate
parseAipDate =
  AipDate <$> parseDay <* char '-' <*> parseMonth <* char '-' <*> parseYear

instance HasDay AipDate where
  day =
    aipday . day
    
instance HasMonth AipDate where
  month =
    aipmonth . month

instance HasYear AipDate where
  year =
    aipyear . year

uriAipDate ::
  AipDate
  -> String
uriAipDate (AipDate (Day d1 d2) m (Year y1 y2 y3 y4)) =
  concat
    [
      show d1
    , show d2
    , "-"
    , show m
    , "-"
    , show y1
    , show y2
    , show y3
    , show y4
    ]

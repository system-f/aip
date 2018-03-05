{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.AipDate(
  AipDateSeparator(..)
, AsAipDateSeparator(..)
, parseAipDateSeparator
, uriAipDateSeparator
, AipDate(..)
, parseAipDate
, uriAipDate
, HasAipDate(..)
) where

import Data.Aviation.Aip.Day(Day(Day), HasDay(day), parseDay)
import Data.Aviation.Aip.Month(Month, HasMonth(month), parseMonth)
import Data.Aviation.Aip.Year(Year(Year), HasYear(year), parseYear)
import Text.Parser.Char(CharParsing, char)
import Papa

data AipDateSeparator =
  NoAipDateSeparator
  | HyphenAipDateSeparator
  | SpaceAipDateSeparator
  deriving (Eq, Ord, Show)

makeClassyPrisms ''AipDateSeparator

parseAipDateSeparator ::
  (CharParsing p, Monad p) =>
  p AipDateSeparator
parseAipDateSeparator =
  HyphenAipDateSeparator <$ char '-' <|>
  SpaceAipDateSeparator <$ char ' ' <|>
  pure NoAipDateSeparator

uriAipDateSeparator ::
  AipDateSeparator
  -> String
uriAipDateSeparator NoAipDateSeparator =
  ""
uriAipDateSeparator HyphenAipDateSeparator =
  "-"
uriAipDateSeparator SpaceAipDateSeparator =
  " "

data AipDate =
  AipDate {
    _aipday ::
      Day
  , separator1 ::
      AipDateSeparator
  , _aipmonth ::
      Month
  , separator2 ::
      AipDateSeparator
  , _aipyear ::
      Year
  } deriving (Eq, Ord, Show)

makeClassy ''AipDate

parseAipDate ::
  (CharParsing p, Monad p) =>
  p AipDate
parseAipDate =
  AipDate <$> parseDay <*> parseAipDateSeparator <*> parseMonth <*> parseAipDateSeparator <*> parseYear

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
uriAipDate (AipDate (Day d1 d2) s1 m s2 (Year y1 y2 y3 y4)) =
  concat
    [
      show d1
    , show d2
    , uriAipDateSeparator s1
    , show m
    , uriAipDateSeparator s2
    , show y1
    , show y2
    , show y3
    , show y4
    ]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.AipHref(
  AipHref(..)
, parseAipHref
, uriAipHref
, HasAipHref(..)
) where

import Data.Aviation.Aip.AipDate(AipDate, HasAipDate(aipDate), parseAipDate)
import Data.Aviation.Aip.AipPg(AipPg, HasAipPg(aipPg), parseAipPg, aippg1, aippg2)
import Data.Aviation.Aip.Day(HasDay(day, day1, day2))
import Data.Aviation.Aip.Month(HasMonth(month))
import Data.Aviation.Aip.Year(HasYear(year, year1, year2, year3, year4))
import Data.Digit(Digit, HasDigit(hasdigit), parsedigit)
import Text.Parser.Char(CharParsing, string, char)
import Papa

data AipHref =
  AipHref {
    _aiphrefpg ::
       AipPg
  , _aiphrefdate ::
       AipDate
  , _aiphrefversion ::
       Digit
  } deriving (Eq, Ord, Show)

makeClassy ''AipHref

parseAipHref ::
  (CharParsing p, Monad p) =>
  p AipHref
parseAipHref =
  let amp = char '&' <* optional (string "amp;")
  in  string "aip.asp?pg=" *> 
        (AipHref <$> parseAipPg <* amp <* string "vdate=" <*> parseAipDate <* amp <* string "ver=" <*> parsedigit)

uriAipHref ::
  HasAipHref s =>
  s
  -> String
uriAipHref ahref =
  concat
    [
      "?pg="
    , show (ahref ^. aipHref . aippg1)
    , show (ahref ^. aipHref . aippg2)
    , "&vdate="
    , show (ahref ^. aipHref . day1)
    , show (ahref ^. aipHref . day2)
    , "-"
    , show (ahref ^. aipHref . month)
    , "-"
    , show (ahref ^. aipHref . year1)
    , show (ahref ^. aipHref . year2)
    , show (ahref ^. aipHref . year3)
    , show (ahref ^. aipHref . year4)
    , "&ver="
    , show (ahref ^. aipHref . hasdigit)
    ]

instance HasAipPg AipHref where
  aipPg =
    aiphrefpg . aipPg
    
instance HasAipDate AipHref where
  aipDate =
    aiphrefdate . aipDate
    
instance HasDigit AipHref where
  hasdigit =
    aiphrefversion . hasdigit
    
instance HasDay AipHref where
  day =
    aipDate . day
    
instance HasMonth AipHref where
  month =
    aipDate . month

instance HasYear AipHref where
  year =
    aipDate . year
    
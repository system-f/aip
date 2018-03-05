{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.Month(
  Month(..)
, parseMonth
, HasMonth(..)
, AsMonth(..)
) where

import Text.Parser.Combinators(choice, try)
import Text.Parser.Char(CharParsing, string)
import Papa

data Month =
  Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Eq, Ord, Show)

parseMonth ::
  CharParsing p =>
  p Month
parseMonth =
  choice
    [
      Jan <$ string "Jan"
    , Feb <$ try (string "Feb")
    , Feb <$ try (string "FEB")
    , Mar <$ try (string "Mar")
    , Mar <$ try (string "MAR")
    , Apr <$ try (string "Apr")
    , Apr <$ try (string "APR")
    , May <$ try (string "May")
    , May <$ try (string "MAY")
    , Jun <$ try (string "Jun")
    , Jun <$ try (string "JUN")
    , Jul <$ try (string "Jul")
    , Jul <$ try (string "JUL")
    , Aug <$ try (string "Aug")
    , Aug <$ try (string "AUG")
    , Sep <$ try (string "Sep")
    , Sep <$ try (string "SEP")
    , Oct <$ try (string "Oct")
    , Oct <$ try (string "OCT")
    , Nov <$ try (string "Nov")
    , Nov <$ try (string "NOV")
    , Dec <$ try (string "Dec")
    , Dec <$ try (string "DEC")
    ]

makeClassy ''Month
makeClassyPrisms ''Month

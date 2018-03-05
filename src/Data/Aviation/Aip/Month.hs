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
  | JAN
  | Feb
  | FEB
  | Mar
  | MAR
  | Apr
  | APR
  | May
  | MAY
  | Jun
  | JUN
  | Jul
  | JUL
  | Aug
  | AUG
  | Sep
  | SEP
  | Oct
  | OCT
  | Nov
  | NOV
  | Dec
  | DEC
  deriving (Eq, Ord, Show)

parseMonth ::
  CharParsing p =>
  p Month
parseMonth =
  choice
    [
      Jan <$ string "Jan"
    , Feb <$ try (string "Feb")
    , FEB <$ try (string "FEB")
    , Mar <$ try (string "Mar")
    , MAR <$ try (string "MAR")
    , Apr <$ try (string "Apr")
    , APR <$ try (string "APR")
    , May <$ try (string "May")
    , MAY <$ try (string "MAY")
    , Jun <$ try (string "Jun")
    , JUN <$ try (string "JUN")
    , Jul <$ try (string "Jul")
    , JUL <$ try (string "JUL")
    , Aug <$ try (string "Aug")
    , AUG <$ try (string "AUG")
    , Sep <$ try (string "Sep")
    , SEP <$ try (string "SEP")
    , Oct <$ try (string "Oct")
    , OCT <$ try (string "OCT")
    , Nov <$ try (string "Nov")
    , NOV <$ try (string "NOV")
    , Dec <$ try (string "Dec")
    , DEC <$ try (string "DEC")
    ]

makeClassy ''Month
makeClassyPrisms ''Month

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

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
    , Mar <$ try (string "Mar")
    , Apr <$ try (string "Apr")
    , May <$ try (string "May")
    , Jun <$ try (string "Jun")
    , Jul <$ try (string "Jul")
    , Aug <$ try (string "Aug")
    , Sep <$ try (string "Sep")
    , Oct <$ try (string "Oct")
    , Nov <$ try (string "Nov")
    , Dec <$ try (string "Dec")
    ]

makeClassy ''Month
makeClassyPrisms ''Month

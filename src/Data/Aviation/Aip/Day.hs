{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.Day(
  Day(..)
, parseDay
, HasDay(..)
) where

import Data.Digit(Digit, parsedigit)
import Text.Parser.Char(CharParsing)
import Papa

data Day =
  Day {
    _day1 ::
      Digit
  , _day2 ::
      Digit
  } deriving (Eq, Ord, Show)

parseDay ::
  (CharParsing p, Monad p) =>
  p Day
parseDay =
  Day <$> parsedigit <*> parsedigit

makeClassy ''Day

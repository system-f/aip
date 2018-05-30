{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.AipPg where

import Data.Digit(Digit, parseDecimal)
import Text.Parser.Char(CharParsing, string)
import Papa

data AipPg =
  AipPg {
    _aippg1 ::
      Digit
  , _aippg2 ::
      Digit
  } deriving (Eq, Ord, Show)

makeClassy ''AipPg

parseAipPg ::
  (CharParsing p, Monad p) =>
  p AipPg
parseAipPg =
  AipPg <$> parseDecimal <*> parseDecimal

parseAipPgHref ::
  (CharParsing p, Monad p) =>
  p AipPg
parseAipPgHref =
  string "aip.asp?pg=" *> 
  parseAipPg

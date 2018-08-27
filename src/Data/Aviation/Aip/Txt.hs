{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.Txt(
  Txt(..)
, dropTxtFile
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Papa hiding ((.=))

newtype Txt =
  Txt
    String
  deriving (Eq, Ord, Show)

instance FromJSON Txt where
  parseJSON v =
    Txt <$> parseJSON v

instance ToJSON Txt where
  toJSON (Txt x) =
    toJSON x

instance Semigroup Txt where
  Txt x <> Txt y =
    Txt (x <> y)

instance Monoid Txt where
  mappend =
    (<>)
  mempty =
    Txt mempty

instance Wrapped Txt where
  type Unwrapped Txt = String
  _Wrapped' =
    iso
      (\(Txt x) -> x)
      Txt

instance Txt ~ a =>
  Rewrapped Txt a

dropTxtFile ::
  Txt
  -> Txt
dropTxtFile =
  (_Wrapped %~ reverse . dropWhile (/= '/') . reverse)

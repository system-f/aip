{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.AipRecords(
  AipRecords(..)
) where

import Data.Aeson(FromJSON(parseJSON), ToJSON(toJSON))
import Data.Aviation.Aip.AipRecord(AipRecord, ManyAipRecord(_ManyAipRecord))
import Papa

newtype AipRecords =
  AipRecords
    [AipRecord]
  deriving (Eq, Show)

instance Monoid AipRecords where
  mempty =
    AipRecords mempty
  AipRecords x `mappend` AipRecords y =
    AipRecords (x `mappend` y)

instance Cons AipRecords AipRecords AipRecord AipRecord where
  _Cons =
    prism'
      (\(h, AipRecords t) -> AipRecords (h `cons` t))
      (\(AipRecords x) -> fmap (fmap AipRecords) (uncons x))

instance AsEmpty AipRecords where
  _Empty =
    prism'
      (\() -> AipRecords [])
      (\(AipRecords x) -> case x of
                            [] ->
                              Just ()
                            _:_ ->
                              Nothing)

instance FromJSON AipRecords where
  parseJSON v =
    AipRecords <$> parseJSON v

instance ToJSON AipRecords where
  toJSON (AipRecords x) =
    toJSON x

instance ManyAipRecord AipRecords where
  _ManyAipRecord f (AipRecords x) =
    AipRecords <$> traverse f x

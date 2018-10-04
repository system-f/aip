{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.OnAipRecords(
  OnAipRecords(..)
, ioOnAipRecords
, nothingOnAipRecords
, resultOnAipRecords
, downloaddirOnAipRecords
, basedirOnAipRecords
, aipRecordsOnAipRecords
, aipRecordsTimeOnAipRecords
, aipRecordsTimesOnAipRecords
, printOnAipRecords
, OnAipRecordsAipCon
, OnAipRecordsIO
) where

import Control.Category((.))
import Control.Applicative(Applicative(pure, (<*>)), Alternative((<|>), empty))
import Control.Lens hiding ((<.>))
import Control.Monad(Monad(return, (>>=)))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Aviation.Aip.AipCon(AipCon)
import Data.Aviation.Aip.AipRecord(aipRecordTime)
import Data.Aviation.Aip.AipRecords(AipRecords, aipRecords1)
import Data.Either(Either(Left, Right), either)
import Data.Foldable(toList)
import Data.Functor(Functor(fmap))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.List.NonEmpty(NonEmpty)
import Data.Time(UTCTime)
import System.FilePath(FilePath)
import System.IO(IO, print, putStrLn)
import Control.Exception(IOException)

newtype OnAipRecords f a =
  OnAipRecords
    (Either IOException (FilePath, AipRecords) -> FilePath -> f a)

instance Functor f => Functor (OnAipRecords f) where
  fmap f (OnAipRecords x) =
    OnAipRecords (\h d -> fmap f (x h d))

instance Apply f => Apply (OnAipRecords f) where
  OnAipRecords f <.> OnAipRecords a =
    OnAipRecords (\h d -> f h d <.> a h d)

instance Applicative f => Applicative (OnAipRecords f) where
  pure =
    OnAipRecords . pure . pure . pure

  OnAipRecords f <*> OnAipRecords a =
    OnAipRecords (\h d -> f h d <*> a h d)

instance Bind f => Bind (OnAipRecords f) where
  OnAipRecords x >>- f =
    OnAipRecords (\h d -> x h d >>- \a -> let g = f a ^. _Wrapped in g h d)

instance Monad f => Monad (OnAipRecords f) where
  return =
    pure
  OnAipRecords x >>= f =
    OnAipRecords (\h d -> x h d >>= \a -> let g = f a ^. _Wrapped in g h d)

instance Alt f => Alt (OnAipRecords f) where
  OnAipRecords x <!> OnAipRecords y =
    OnAipRecords (\h d -> x h d <!> y h d)

instance Alternative f => Alternative (OnAipRecords f) where
  OnAipRecords x <|> OnAipRecords y =
    OnAipRecords (\h d -> x h d <|> y h d)
  empty =
    (OnAipRecords . pure . pure) empty

instance MonadTrans OnAipRecords where
  lift =
    OnAipRecords . pure . pure

instance MonadIO f => MonadIO (OnAipRecords f) where
  liftIO =
    OnAipRecords . pure . pure . liftIO

instance OnAipRecords f a ~ x =>
  Rewrapped (OnAipRecords g k) x

instance Wrapped (OnAipRecords f k) where
  type Unwrapped (OnAipRecords f k) =
      Either IOException (FilePath, AipRecords)
      -> FilePath
      -> f k
  _Wrapped' =
    iso
      (\(OnAipRecords x) -> x)
      OnAipRecords

ioOnAipRecords ::
  MonadIO f =>
  (Either IOException (FilePath, AipRecords) -> FilePath -> IO a)
  -> OnAipRecords f a
ioOnAipRecords k =
  OnAipRecords (\h d -> liftIO (k h d))

nothingOnAipRecords ::
  Applicative f =>
  OnAipRecords f ()
nothingOnAipRecords =
  pure ()

resultOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException (FilePath, AipRecords))
resultOnAipRecords =
  OnAipRecords (\h _ -> pure h)

printOnAipRecords ::
  OnAipRecordsIO ()
printOnAipRecords =
  OnAipRecords (\h d ->
    do  case h of
          Left e ->
            print e
          Right (b, r) ->
            do  print r
                putStrLn b
        putStrLn d)

basedirOnAipRecords ::
  Applicative f =>
  OnAipRecords f FilePath
basedirOnAipRecords =
  OnAipRecords (\_ d -> pure d)

downloaddirOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException FilePath)
downloaddirOnAipRecords =
  OnAipRecords (\e _ -> pure (fmap (view _1) e))

aipRecordsOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException AipRecords)
aipRecordsOnAipRecords =
  OnAipRecords (\e _ -> pure (fmap (view _2) e))

aipRecordsTimeOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException (NonEmpty UTCTime))
aipRecordsTimeOnAipRecords =
  fmap (fmap (fmap (view aipRecordTime) . view aipRecords1)) aipRecordsOnAipRecords

aipRecordsTimesOnAipRecords ::
  Applicative f =>
  OnAipRecords f [UTCTime]
aipRecordsTimesOnAipRecords =
  fmap (either (pure []) toList) aipRecordsTimeOnAipRecords

type OnAipRecordsAipCon a =
  OnAipRecords AipCon a

type OnAipRecordsIO a =
  OnAipRecords IO a

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Aip.OnAipRecords(
  OnAipRecords(..)
, ioOnAipRecords
, nothingOnAipRecords
, resultOnAipRecords
, logOnAipRecords
, downloaddirOnAipRecords
, basedirOnAipRecords
, aipRecordsOnAipRecords
, prefixedAipRecordsOnAipRecords
, aipRecordsTimeOnAipRecords
, aipRecordsTimesOnAipRecords
, printOnAipRecords
, OnAipRecordsAipCon
, OnAipRecordsIO
, logeachOnAipRecords
, logShowOnAipRecords
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
import Data.Aviation.Aip.Href(aipPrefix)
import Data.Either(Either(Left, Right), either)
import Data.Foldable(toList)
import Data.Functor(Functor(fmap))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.List.NonEmpty(NonEmpty)
import Data.String(String)
import Data.Time(UTCTime)
import Prelude(Show(show))
import System.FilePath(FilePath)
import System.IO(IO, print, putStrLn)
import Control.Exception(IOException)

newtype OnAipRecords f a =
  OnAipRecords
    (Either IOException (FilePath, AipRecords) -> (String -> IO ()) -> FilePath -> f a)

instance Functor f => Functor (OnAipRecords f) where
  fmap f (OnAipRecords x) =
    OnAipRecords (\h l d -> fmap f (x h l d))

instance Apply f => Apply (OnAipRecords f) where
  OnAipRecords f <.> OnAipRecords a =
    OnAipRecords (\h l d -> f h l d <.> a h l d)

instance Applicative f => Applicative (OnAipRecords f) where
  pure =
    OnAipRecords . pure . pure . pure . pure

  OnAipRecords f <*> OnAipRecords a =
    OnAipRecords (\h l d -> f h l d <*> a h l d)

instance Bind f => Bind (OnAipRecords f) where
  OnAipRecords x >>- f =
    OnAipRecords (\h l d -> x h l d >>- \a -> let g = f a ^. _Wrapped in g h l d)

instance Monad f => Monad (OnAipRecords f) where
  return =
    pure
  OnAipRecords x >>= f =
    OnAipRecords (\h l d -> x h l d >>= \a -> let g = f a ^. _Wrapped in g h l d)

instance Alt f => Alt (OnAipRecords f) where
  OnAipRecords x <!> OnAipRecords y =
    OnAipRecords (\h l d -> x h l d <!> y h l d)

instance Alternative f => Alternative (OnAipRecords f) where
  OnAipRecords x <|> OnAipRecords y =
    OnAipRecords (\h l d -> x h l d <|> y h l d)
  empty =
    (OnAipRecords . pure . pure . pure) empty

instance MonadTrans OnAipRecords where
  lift =
    OnAipRecords . pure . pure . pure

instance MonadIO f => MonadIO (OnAipRecords f) where
  liftIO =
    OnAipRecords . pure . pure . pure . liftIO

instance OnAipRecords f a ~ x =>
  Rewrapped (OnAipRecords g k) x

instance Wrapped (OnAipRecords f k) where
  type Unwrapped (OnAipRecords f k) =
      Either IOException (FilePath, AipRecords)
      -> (String -> IO ())
      -> FilePath
      -> f k
  _Wrapped' =
    iso
      (\(OnAipRecords x) -> x)
      OnAipRecords

ioOnAipRecords ::
  MonadIO f =>
  (Either IOException (FilePath, AipRecords) -> (String -> IO ()) -> FilePath -> IO a)
  -> OnAipRecords f a
ioOnAipRecords k =
  OnAipRecords (\h l d -> liftIO (k h l d))

nothingOnAipRecords ::
  Applicative f =>
  OnAipRecords f ()
nothingOnAipRecords =
  pure ()

resultOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException (FilePath, AipRecords))
resultOnAipRecords =
  OnAipRecords (\h _ _ -> pure h)

logOnAipRecords ::
  Applicative f =>
  OnAipRecords f (String -> IO ())
logOnAipRecords =
  OnAipRecords (\_ l _ -> pure l)

printOnAipRecords ::
  OnAipRecordsIO ()
printOnAipRecords =
  OnAipRecords (\h _ d ->
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
  OnAipRecords (\_ _ d -> pure d)

downloaddirOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException FilePath)
downloaddirOnAipRecords =
  OnAipRecords (\e _ _ -> pure (fmap (view _1) e))

aipRecordsOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException AipRecords)
aipRecordsOnAipRecords =
  OnAipRecords (\e _ _ -> pure (fmap (view _2) e))

prefixedAipRecordsOnAipRecords ::
  Applicative f =>
  OnAipRecords f (Either IOException AipRecords)
prefixedAipRecordsOnAipRecords =
  fmap (fmap aipPrefix) aipRecordsOnAipRecords

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

logeachOnAipRecords ::
  OnAipRecordsIO ()
logeachOnAipRecords =
  OnAipRecords (\h l d ->
    do  l (either show show h)
        l (show d))

logShowOnAipRecords ::
  Show a =>
  a
  -> OnAipRecordsIO ()
logShowOnAipRecords z =
  do  l <- logOnAipRecords
      lift (l (show z))

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Aip.AipCon(
  AipCon(..)
, islog
) where

import Control.Category((.))
import Control.Applicative(Applicative(pure, (<*>)))
import Control.Lens
import Control.Monad(Monad(return, (>>=)))
import Control.Monad.Catch(MonadThrow(throwM), MonadCatch(catch))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Except(ExceptT)
import Data.Aviation.Aip.ConnErrorHttp4xx(ConnErrorHttp4xx)
import Data.Bool(Bool)
import Data.Functor(Functor(fmap))
import System.IO(IO)

newtype AipCon a =
  AipCon (Bool -> ExceptT ConnErrorHttp4xx IO a)

instance AipCon a ~ r =>
  Rewrapped (AipCon b) r

instance Wrapped (AipCon x) where
  type Unwrapped (AipCon x) =
    Bool
    -> ExceptT ConnErrorHttp4xx IO x
  _Wrapped' =
    iso
      (\(AipCon x) -> x)
      AipCon

instance Functor AipCon where
  fmap f (AipCon x) =
    AipCon (fmap (fmap f) x)

instance Applicative AipCon where
  pure =
    AipCon . pure . pure
  AipCon f <*> AipCon a =
    AipCon (\b -> f b <*> a b)

instance Monad AipCon where
  return =
    pure
  AipCon x >>= f =
    AipCon (\b -> x b >>= \a -> let r = f a ^. _Wrapped in r b)

instance MonadIO AipCon where
  liftIO =
    AipCon . pure . liftIO

instance MonadThrow AipCon where
  throwM e =
    AipCon (\_ -> throwM e)

instance MonadCatch AipCon where
  catch (AipCon x) k =
    AipCon (\b -> catch (x b) (\z -> let r = k z ^. _Wrapped in r b))

islog ::
  AipCon Bool
islog =
  AipCon pure

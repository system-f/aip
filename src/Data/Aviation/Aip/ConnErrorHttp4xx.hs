{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.ConnErrorHttp4xx(
  ConnErrorHttp4xx(..)
, AipConn
, AsConnErrorHttp4xx(..)
, FoldConnErrorHttp4xx(..)
, GetConnErrorHttp4xx(..)
, SetConnErrorHttp4xx(..)
, ManyConnErrorHttp4xx(..)
, HasConnErrorHttp4xx(..)
, IsConnErrorHttp4xx(..)
) where

import Control.Monad.Trans.Except(ExceptT)
import Network.Stream(ConnError)
import Papa

data ConnErrorHttp4xx =
  IsConnError ConnError
  | Http4xx Int Int
  deriving (Eq, Show)

type AipConn a =
  ExceptT ConnErrorHttp4xx IO a

class AsConnErrorHttp4xx a where
  _ConnErrorHttp4xx ::
    Prism' a ConnErrorHttp4xx
  default _ConnErrorHttp4xx ::
    IsConnErrorHttp4xx a =>
    Prism' a ConnErrorHttp4xx
  _ConnErrorHttp4xx =
    _IsConnErrorHttp4xx
    
instance AsConnErrorHttp4xx ConnErrorHttp4xx where
  _ConnErrorHttp4xx =
    id

class FoldConnErrorHttp4xx a where
  _FoldConnErrorHttp4xx ::
    Fold a ConnErrorHttp4xx
    
instance FoldConnErrorHttp4xx ConnErrorHttp4xx where
  _FoldConnErrorHttp4xx =
    id

class FoldConnErrorHttp4xx a => GetConnErrorHttp4xx a where
  _GetConnErrorHttp4xx ::
    Getter a ConnErrorHttp4xx
  default _GetConnErrorHttp4xx ::
    HasConnErrorHttp4xx a =>
    Getter a ConnErrorHttp4xx
  _GetConnErrorHttp4xx =
    connErrorHttp4xx
    
instance GetConnErrorHttp4xx ConnErrorHttp4xx where
  _GetConnErrorHttp4xx =
    id

class SetConnErrorHttp4xx a where
  _SetConnErrorHttp4xx ::
    Setter' a ConnErrorHttp4xx
  default _SetConnErrorHttp4xx ::
    ManyConnErrorHttp4xx a =>
    Setter' a ConnErrorHttp4xx
  _SetConnErrorHttp4xx =
    _ManyConnErrorHttp4xx

instance SetConnErrorHttp4xx ConnErrorHttp4xx where
  _SetConnErrorHttp4xx =
    id

class (FoldConnErrorHttp4xx a, SetConnErrorHttp4xx a) => ManyConnErrorHttp4xx a where
  _ManyConnErrorHttp4xx ::
    Traversal' a ConnErrorHttp4xx

instance ManyConnErrorHttp4xx ConnErrorHttp4xx where
  _ManyConnErrorHttp4xx =
    id

class (GetConnErrorHttp4xx a, ManyConnErrorHttp4xx a) => HasConnErrorHttp4xx a where
  connErrorHttp4xx ::
    Lens' a ConnErrorHttp4xx
  default connErrorHttp4xx ::
    IsConnErrorHttp4xx a =>
    Lens' a ConnErrorHttp4xx
  connErrorHttp4xx =
    _IsConnErrorHttp4xx

instance HasConnErrorHttp4xx ConnErrorHttp4xx where
  connErrorHttp4xx =
    id

class (HasConnErrorHttp4xx a, AsConnErrorHttp4xx a) => IsConnErrorHttp4xx a where
  _IsConnErrorHttp4xx ::
    Iso' a ConnErrorHttp4xx
    
instance IsConnErrorHttp4xx ConnErrorHttp4xx where
  _IsConnErrorHttp4xx =
    id

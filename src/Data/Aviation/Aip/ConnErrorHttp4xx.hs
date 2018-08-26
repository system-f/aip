{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.ConnErrorHttp4xx(
  ConnErrorHttp4xx(..)
, AipConn
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

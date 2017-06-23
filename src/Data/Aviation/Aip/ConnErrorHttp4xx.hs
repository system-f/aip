{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Aip.ConnErrorHttp4xx(
  ConnErrorHttp4xx(..)
) where

import Network.Stream(ConnError)
import Papa

data ConnErrorHttp4xx =
  IsConnError ConnError
  | Http4xx Int Int
  deriving (Eq, Show)

makeClassyPrisms ''ConnErrorHttp4xx

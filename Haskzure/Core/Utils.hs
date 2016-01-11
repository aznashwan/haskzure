{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

{-|
Module      : Utils
Description : General utilities package.
Copyright   : (c) Nashwan Azhari, 2016
License     : Apache 2.0
Maintainer  : aznashwan@yahoo.com
Stability   : experimental
Portability : POSIX, Win32

Utils defines some useful utilities as well as some
sad yet necessary orphan instances:
-}
module Haskzure.Core.Utils (
        )where

import           Data.Aeson         (FromJSON (..), ToJSON (..))
import qualified Data.Aeson         as Aes

import           Data.ByteString    (ByteString)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | Utils defines some useful utilities as well as some
-- sad yet necessary orphan instances:


-- Naive n' orphaned instances for ToJSON and FromJSON for ByteStrings...
instance FromJSON ByteString where
    parseJSON (Aes.String t) = return $ encodeUtf8 t
    parseJSON _ = fail "ByteString must be decoded from String"

instance ToJSON ByteString where
    toJSON s = Aes.String $ decodeUtf8 s

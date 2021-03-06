{-|
 Module      : Cloud.Haskzure
 Description : Azure Resource Manager bindings in Haskell.
 Copyright   : (c) Nashwan Azhari, 2016
 License     : Apache 2.0
 Maintainer  : aznashwan@yahoo.com
 Stability   : experimental
 Portability : POSIX, Win32

 Haskzure containings bindings to be used with the new Azure Resource Manager APIs.
-}

module Cloud.Haskzure (
    -- * Core components:
    AzureResource(..),

    -- * Authorization:
    Credentials(..),
    Token(..),
    getToken,

    -- * Oprations:
    createOrUpdate, get, delete,

    -- * Instance Generation helpers and utilities:
    mkJSONInsts, azureResourceInsts, azureResourceInst, toJSONInst,
    fromJSONInst, monoidInst, recordFieldsInfo
    )where

import           Cloud.Haskzure.Core
import           Cloud.Haskzure.Gen

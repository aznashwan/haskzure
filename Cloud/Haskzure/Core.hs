{-|
 Module      : Core
 Description : Core namespace module.
 Copyright   : (c) Nashwan Azhari, 2016
 License     : Apache 2.0
 Maintainer  : aznashwan@yahoo.com
 Stability   : experimental
 Portability : POSIX, Win32

 Core contains the core components of the project.
-}

module Cloud.Haskzure.Core (
    -- ** The AzureResource typeclass.
    AzureResource(..),

    -- ** The Resource ADT.
    Resource(..),

    -- ** Authentication data strutures and functions:
    Credentials(..),
    Token(..),
    getToken, createOrUpdate, get, delete
  ) where

import           Cloud.Haskzure.Core.Auth
import           Cloud.Haskzure.Core.AzureResource
import           Cloud.Haskzure.Core.Operations
import           Cloud.Haskzure.Core.Resource

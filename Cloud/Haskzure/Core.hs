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
  ) where

import           Cloud.Haskzure.Core.Resource (AzureResource (..),
                                               Resource (..))

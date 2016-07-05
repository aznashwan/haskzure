{-|
 Module      : AzureResource
 Description :
 Copyright   : (c) Nashwan Azhari, 2016
 License     : Apache 2.0
 Maintainer  : Nashwan Azhari <aznashwan@yahoo.com>
 Stability   : experimental
 Portability : POSIX, Win32

 This module contains the definition of  the AzureResource
 typeclass to which all Azure resource datatypes must comply.
-}


{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Trustworthy           #-}


module Cloud.Haskzure.Core.AzureResource (
  AzureResource(..),
  )where


import           Data.Aeson (FromJSON (..), ToJSON (..))


-- The definition of  the AzureResource typeclass to which all Azure resource datatypes must comply.


-- | AzureResource is the typeclass to which all AzureResource
-- resource datatypes must comply in order to be deployed.
-- It should be directly serializable to/from JSON.
class (ToJSON r, FromJSON r) => AzureResource r where
  -- | rID returns the String ID of the AzureResource:
  rID :: r    -- ^ the AzureResource instance.
      -> String -- ^ the String ID of the AzureResource.

  -- | rName returns the String name of the AzureResource:
  rName :: r    -- ^ the AzureResource instance.
        -> String -- ^ the String name of the AzureResource as present on Azure.

  -- | rLocation returns the normalized String location of the AzureResource:
  rLocation :: r    -- ^ the AzureResource instance.
            -> String -- ^ the String location of the AzureResource.

  -- | rType returns the String Type of the AzureResource in 'Provider/ResourceType' form:
  rType :: r    -- ^ the AzureResource instance.
        -> String -- ^ the String type of the AzureResource as present on Azure.

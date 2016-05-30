{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Trustworthy            #-}

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
module Cloud.Haskzure.Core.AzureResource (
  AzureResource(..),
  )where


import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Text  (Text)


-- This module contains the definition of  the AzureResource
-- typeclass to which all Azure resource datatypes must comply.


-- | AzureResource is the typeclass to which all AzureResource
-- resource datatypes must comply in order to be deployed.
-- It should be directly serializable to/from JSON.
class (ToJSON a, FromJSON a) => AzureResource r a | r -> a where
  -- | rID returns the Text ID of the AzureResource:
  rID :: r    -- ^ the AzureResource instance.
      -> Text -- ^ the String ID of the AzureResource.

  -- | rName returns the Text name of the AzureResource:
  rName :: r    -- ^ the AzureResource instance.
        -> Text -- ^ the String name of the AzureResource as present on Azure.

  -- | rLocation returns the normalized String location of the AzureResource:
  rLocation :: r    -- ^ the AzureResource instance.
            -> Text -- ^ the String location of the AzureResource.

  -- | rType returns the String Type of the AzureResource in 'Provider/ResourceType' form:
  rType :: r    -- ^ the AzureResource instance.
        -> Text -- ^ the String type of the AzureResource as present on Azure.

  -- | rProperties returns the set of properties specific to this AzureResource:
  rProperties :: r -- ^ the AzureResource instance.
              -> a -- ^ the AzureResource's specific properties.

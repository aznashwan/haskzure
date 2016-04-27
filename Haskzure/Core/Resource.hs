{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Trustworthy           #-}

{-|
Module      : Resource
Description : Definition for core resource type.
Copyright   : (c) Nashwan Azhari, 2016
License     : Apache 2.0
Maintainer  : aznashwan@yahoo.com
Stability   : experimental
Portability : POSIX, Win32

Resource defines the core Resource ATD which will be used to model
Azure resources. Resource is an instance of AzureResource.
-}
module Haskzure.Core.Resource (
  AzureResource(..),
  Resource(..),
  def
  )where


import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              Value (..), object, (.:), (.=))
import           Data.Default                (Default, def)
import           Data.Text                   (Text)

import           Haskzure.Core.AzureResource (AzureResource (..))
import           Haskzure.Core.Utils         ()


-- | Resource defines the core Resource ATD which will be used
-- to model Azure resources. Resource is an instance of AzureResource.


-- | Resource is the basic implementation of a generic Azure resource:
data Resource a = Resource {
    -- | resID is the ID of the Resource:
    resID         :: Text,

    -- | resName is the Name of the Resource:
    resName       :: Text,

    -- | resLocation is the Location of the Resource:
    resLocation   :: Text,

    -- | resType is the Type of Resource:
    resType       :: Text,

    -- | resProperties are the properties of the Resource:
    resProperties :: a
} deriving (Show, Eq)

-- Default instance for a Resource
instance Default (Resource Value) where
    def = Resource {
        resID = "",
        resName = "",
        resLocation = "",
        resType = "",
        resProperties = Null
    }

-- AzureResource instance for Resource:
instance (ToJSON a, FromJSON a) => AzureResource (Resource a) a where
    rID = resID
    rName = resName
    rLocation = resLocation
    rType = resType
    rProperties = resProperties

-- FromJSON instance for Resource:
instance (FromJSON a) => FromJSON (Resource a) where
    parseJSON (Object o) = Resource <$>
        o .: "id" <*> o .: "name" <*> o .: "location" <*> o .: "type" <*> o .: "properties"
    parseJSON _ = fail "Expected a Resource (which is a JSON Object)."

-- ToJSON instance for Resource:
instance (ToJSON a) => ToJSON (Resource a) where
    toJSON (Resource iD name location typ props) = object [
            "id" .= iD,
            "name" .= name,
            "location" .= location,
            "type" .= typ,
            "properties" .= props
           ]

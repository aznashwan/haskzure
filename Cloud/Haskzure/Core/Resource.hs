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

{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Trustworthy           #-}


module Cloud.Haskzure.Core.Resource (
  Resource(..)
  )where


import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    Value (..), object, pairs,
                                                    (.:), (.=))
import           Data.Default                      (Default, def)
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)

import           Cloud.Haskzure.Core.AzureResource (AzureResource (..))
import           Cloud.Haskzure.Core.Utils         ()


-- | Resource defines the core Resource ATD which will be used
-- to model Azure resources. Resource is an instance of AzureResource.


-- | Resource is the basic implementation of a generic Azure resource:
data Resource a = Resource {
    -- | resID is the ID of the Resource:
    resID         :: String,

    -- | resName is the Name of the Resource:
    resName       :: String,

    -- | resGroup is the Name of the Resource Group in which the Resource lives.
    resGroup      :: String,

    -- | resLocation is the Location of the Resource:
    resLocation   :: String,

    -- | resType is the Type of Resource:
    resType       :: String,

    -- | resProperties are the properties of the Resource:
    resProperties :: a
} deriving (Show)


instance Eq (Resource a) where
    r == s = map ($ r) eqfs == map ($ s) eqfs
        where eqfs = [resID, resName, resLocation, resGroup, resType]

-- FromJSON instance for Resource:
instance (FromJSON a) => FromJSON (Resource a) where
    -- TODO: group here:
    parseJSON (Object o) = Resource <$>
        o .: "id" <*> o .: "name" <*> (pure $ ("theResourceGroup" :: String)) <*> o .: "location" <*> o .: "type" <*> o .: "properties"
    parseJSON _ = fail "Expected a Resource (which is a JSON Object)."

-- ToJSON instance for Resource:
instance (ToJSON a) => ToJSON (Resource a) where
    -- TODO: group is useless:
    toJSON (Resource iD name group location typ props) = object [
            "id" .= iD,
            "name" .= name,
            "location" .= location,
            "type" .= typ,
            "properties" .= props
        ]
    toEncoding (Resource iD name group location typ props) = pairs (
            "id" .= iD <>
            "name" .= name <>
            "location" .= location <>
            "type" .= typ <>
            "properties" .= props
        )

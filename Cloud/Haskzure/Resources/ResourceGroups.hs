{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cloud.Haskzure.Resources.ResourceGroups (
    ResourceGroup(..)) where


import           GHC.Generics       (Generic)

import           Cloud.Haskzure.Gen (fromJSONInst, monoidInst, toJSONInst)


data ResourceGroup = ResourceGroup {
    resourceGroupLocation :: String
    } deriving (Show, Eq, Generic)

monoidInst ''ResourceGroup
toJSONInst ''ResourceGroup
fromJSONInst ''ResourceGroup

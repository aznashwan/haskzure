{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestInstances (
    prop_AesonSymmetrySimpleDatatype,
    prop_AesonSymmetryNestedDatatype
    ) where


import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromJust)
import           GHC.Generics         (Generic)

import           Data.Aeson           (decode, encode)

import           Cloud.Haskzure.Core  (AzureResource (..))
import           Cloud.Haskzure.Gen   (azureResourceInsts, mkJSONInsts)
import           TestUtils            (arbiInst)


-- | Simple datatype on which to test the basics of the Aeson instances generators.
data SimpleData = SimpleData {
        simpleDataField1 :: String,
        simpleDataField2 :: Int,
        simpleDataField3 :: String
    } deriving (Show, Eq, Generic)

mkJSONInsts ''SimpleData
arbiInst ''SimpleData

-- | Datatype which nests another simple datatype.
data NestedData = NestedData {
        nestedDataField1 :: String,
        nestedDataField2 :: SimpleData
    } deriving (Show, Eq, Generic)

mkJSONInsts ''NestedData
arbiInst ''NestedData

-- | Datatype on which to test AzureResource instance generation.
data TestData = TestData {
    testDataID       :: String,
    testDataName     :: String,
    testDataLocation :: String,
    testDataType     :: String
 } deriving (Show, Eq, Generic)

azureResourceInsts ''TestData

prop_AesonSymmetrySimpleDatatype :: SimpleData -> Bool
prop_AesonSymmetrySimpleDatatype d =
    ((fromJust . (decode :: BSL.ByteString -> Maybe SimpleData) . encode) d) == d

prop_AesonSymmetryNestedDatatype :: NestedData -> Bool
prop_AesonSymmetryNestedDatatype d =
    ((fromJust . (decode :: BSL.ByteString -> Maybe NestedData) . encode) d) == d

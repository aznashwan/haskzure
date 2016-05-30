{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestInstances (
    prop_AesonSymmetrySimpleDatatype,
    prop_AesonSymmetryNestedDatatype
    ) where


import qualified Data.ByteString.Lazy         as BSL
import           Data.Maybe                   (fromJust)
import           GHC.Generics                 (Generic)

import           Data.Aeson                   (decode, encode)

import           Cloud.Haskzure.Gen.Instances (fromJSONInst, monoidInst,
                                               toJSONInst)
import           TestUtils                    (arbiInst)


data SimpleData = SimpleData {
        simpleDataField1 :: String,
        simpleDataField2 :: Int,
        simpleDataField3 :: String
    } deriving (Show, Eq, Generic)

monoidInst ''SimpleData
toJSONInst ''SimpleData
fromJSONInst ''SimpleData
arbiInst ''SimpleData

data NestedData = NestedData {
        nestedDataField1 :: String,
        nestedDataField2 :: SimpleData
    } deriving (Show, Eq, Generic)

monoidInst ''NestedData
toJSONInst ''NestedData
fromJSONInst ''NestedData
arbiInst ''NestedData


prop_AesonSymmetrySimpleDatatype :: SimpleData -> Bool
prop_AesonSymmetrySimpleDatatype d =
    ((fromJust . (decode :: BSL.ByteString -> Maybe SimpleData) . encode) d) == d

prop_AesonSymmetryNestedDatatype :: NestedData -> Bool
prop_AesonSymmetryNestedDatatype d =
    ((fromJust . (decode :: BSL.ByteString -> Maybe NestedData) . encode) d) == d

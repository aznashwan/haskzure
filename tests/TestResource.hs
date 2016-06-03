{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ResourceTests defines QuickCheck tests for the functions
-- and structures in the Cloud.Haskzure.Core.Resource module.
module TestResource(
    prop_ResourceEncodeDecodeIdempotence
  ) where

import           Data.Aeson          (Value, decode, encode)
import           Data.Text           (Text, pack)
import           Test.QuickCheck     (Arbitrary (..), Gen)

import           Cloud.Haskzure.Core (Resource (..))

import           TestUtils           ()


-- | And a naive Arbitrary instance for a Resource:
instance Arbitrary (Resource Value) where
    arbitrary = Resource <$>
        (arbitrary :: Gen Text) <*>
        (arbitrary :: Gen Text) <*>
        (arbitrary :: Gen Text) <*>
        (arbitrary :: Gen Text) <*>
        (arbitrary ::  Gen Value)


-- | prop_ResourceEncodeDecodeIdempotence tests encode/decode
-- impotence on Resources.
prop_ResourceEncodeDecodeIdempotence :: Resource Value -> Bool
prop_ResourceEncodeDecodeIdempotence r = r == r'
     where (Just r') = decode $ encode r

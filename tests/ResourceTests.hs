{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

-- ResourceTests defines HUnit and QuickCheck tests for the functions
-- and structures in the Haskzure.Core.Resource module.
module ResourceTests(
    prop_ResourceEncodeDecodeIdempotence
  ) where

import           Data.Aeson      (Value, decode, encode)
import           Data.Text       (Text, pack)
import           Test.QuickCheck (Arbitrary (..), Gen)

import           Haskzure.Core   (Resource (..))

import           TestUtils       ()


-- | And a naive Arbitrary instance for a Resource:
instance Arbitrary (Resource Value) where
    arbitrary = Resource <$>
        (arbitrary :: Gen Text) <*>
        (arbitrary :: Gen Text) <*>
        (arbitrary :: Gen Text) <*>
        (arbitrary :: Gen Text) <*>
        (arbitrary ::  Gen Value)

-- | And one for Texts:
instance Arbitrary Text where
    arbitrary = pack <$> (arbitrary :: Gen String)

-- | prop_ResourceEncodeDecodeIdempotence tests encode/decode
-- impotence on Resources.
prop_ResourceEncodeDecodeIdempotence :: Resource Value -> Bool
prop_ResourceEncodeDecodeIdempotence r = r == r'
     where (Just r') = decode $ encode r

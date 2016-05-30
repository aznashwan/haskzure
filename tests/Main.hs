-- | Main is the main entry point of the tests:
module Main (main) where


import           Test.QuickCheck (Testable (..), quickCheck)

import           TestInstances   (prop_AesonSymmetryNestedDatatype,
                                  prop_AesonSymmetrySimpleDatatype)
import           TestResource    (prop_ResourceEncodeDecodeIdempotence)
import           TestUtils       (prop_aeson)


main :: IO ()
main = do
    quickCheck prop_aeson
    quickCheck prop_ResourceEncodeDecodeIdempotence
    quickCheck prop_AesonSymmetrySimpleDatatype
    quickCheck prop_AesonSymmetryNestedDatatype

-- | Main is the main entry point of the tests:
module Main (main) where


import           Test.QuickCheck (quickCheck)

import           ResourceTests   (prop_ResourceEncodeDecodeIdempotence)
import           TestUtils       (prop_aeson)

-- simpleTest is just a list of simple Tests:
simpleTests = [
    prop_aeson
   ]

-- | the list of Resource Tests...
resourceTests = [
   prop_ResourceEncodeDecodeIdempotence
 ]


main :: IO ()
main = do
    mapM_ quickCheck simpleTests
    mapM_ quickCheck resourceTests

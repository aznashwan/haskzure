{-|
 Module      : Cloud.Haskzure.Gen
 Description : Tools used for generation of instance declarations.
 Copyright   : (c) Nashwan Azhari, 2016
 License     : Apache 2.0
 Maintainer  : aznashwan@yahoo.com
 Stability   : experimental
 Portability : POSIX, Win32

 Exposes various helpers useful for generation of instance declarations.
-}

module Cloud.Haskzure.Gen (
    -- ** Main instance generation functions:
    azureResourceInsts, mkJSONInsts, azureResourceInst, toJSONInst, fromJSONInst, monoidInst,

    -- ** Instance generation utilities:
    recordFieldsInfo
    ) where


import           Cloud.Haskzure.Gen.Instances
import           Cloud.Haskzure.Gen.Utils     (recordFieldsInfo)

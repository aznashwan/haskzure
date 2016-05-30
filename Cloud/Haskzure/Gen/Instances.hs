{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cloud.Haskzure.Gen.Instances (
    toJSONInst,
    fromJSONInst,
    monoidInst
    )where

import           Data.Monoid                (Monoid (..))
import           Data.String                (IsString (..))
import           Language.Haskell.TH        (Dec, Q, conT, mkName)
import           Language.Haskell.TH.Syntax (Exp (..), Lift (lift), Lit (..),
                                             Name (..), OccName (..))

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             genericParseJSON,
                                             genericToEncoding)
import           Data.Aeson.Types           (Parser)
import           Generics.Deriving.Monoid   (mappenddefault, memptydefault)

import           Cloud.Haskzure.Gen.Utils


-- | Generates a 'ToJSON' instance provided a datatype given by its 'Name'.
--
-- The given datatype MUST have a single value constructor of record type.
-- Also, the data structure MUST be an instance of 'GHC.Generics.Generic'.
--
-- The generated instance relies on 'toEncoding', and all of its fields will be
-- named following the convention that they are named with the WHOLE name of
-- the structure as a prefix as per example:
--
-- @
-- data TestData = TestData {
--  testDataField1 :: Field1Type,
--  testDataField2 :: Field2Type
--  } deriving Generic
-- @
--
-- With the resulting JSON looking like:
--
-- @
-- {
--  "field1": encodingOfField1,
--  "field2": encodingOfField2
-- }
-- @
toJSONInst :: Name -> Q [Dec]
toJSONInst name =
    [d| instance ToJSON $( conT name ) where
            toEncoding = genericToEncoding $ mkEncodingOptions name
      |]

-- | Generates a 'FromJSON' instance provided a datatype given by its 'Name'.
--
-- The given datatype MUST have a single value constructor of record type and
-- be an instance of 'GHC.Generics.Generic'. In addition, the types comprising
-- the fields of the datatype must be an instance of 'Data.Monoid.Monoid' in
-- order to facilitate defaulting.
-- The generated instance acts like the exact inverse of 'toJSONInst', in that
-- the data structure must have all record fields with its name as a prefix,
-- whilst the decoding process expects the JSON fields to be without.
-- For example:
--
-- @
-- {
--  "field1": encodingOfField1,
--  "field2": encodingOfField2
-- }
-- @
--
-- The above is expected to be decoded into the following structure:
--
-- @
-- data TestData = TestData {
--  testDataField1 :: Field1Type,
--  testDataField2 :: Field2Type
--  } deriving Generic
-- @
fromJSONInst :: Name -> Q [Dec]
fromJSONInst name =
    [d| instance FromJSON $( conT name ) where
            parseJSON = (genericParseJSON (mkEncodingOptions name)
                `withDefaults` $( mkFromJSONPairs name )
                ) :: Value -> Parser $( conT name )
      |]


-- | Generates a 'Data.Monoid.Monoid' instance for the datatype with the
-- provided 'Name'.
--
-- The datatype MUST be an instance of 'GHC.Generics.Generic', with the type of
-- all of its contained felds also 'Data.Monoid.Monoid' instances themselves.
monoidInst :: Name -> Q [Dec]
monoidInst name =
    [d| instance Monoid $( conT name ) where
            mempty = memptydefault
            mappend = mappenddefault
      |]


-- | 'Data.Monoid.Monoid' instance for 'Int' mostly used for 'mempty'.
instance Monoid Int where
    mempty = 0
    mappend = (+)


#if MIN_VERSION_template_haskell(2,11,0)
-- | NOTE: these instances are pretty shameful.
-- The Lift instance is for some reason gone from TH...
-- The IsString though is just a pathetic attempt to mediate the bright
-- idea of using OverloadedStrings and template-haskell together...
instance Lift Name where
    lift (Name (OccName s) _) = return (LitE (StringL s))
instance IsString Name where
    fromString = mkName
#endif

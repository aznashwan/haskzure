{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | TestUtils defines a set of general utilities to be used within testing.
module TestUtils (
  arbiInst,
  prop_aeson
  )where


import           Control.Monad              (liftM, replicateM)
import           Data.List                  (intersperse)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Language.Haskell.TH        (Dec, Q, conE, conT, lookupTypeName,
                                             mkName)
import           Language.Haskell.TH.Syntax (Exp (..), Lift (..), Name (..),
                                             Type (..), showName)

import qualified Data.Aeson                 as Aeson
import           Test.QuickCheck            (Arbitrary (..), Gen (..), Property,
                                             collect, frequency, sized)

import           Cloud.Haskzure.Gen         (recordFieldsInfo)


-- | Generates an 'Arbitrary' instance for the data type with the provided 'Name'.
arbiInst :: Name -> Q [Dec]
arbiInst name = do
    records <- recordFieldsInfo (\(n,_,t) -> (n,t)) name
    arbi <- [| arbitrary |]
    ap <- [| (<*>) |]
    op <- [| (<$>) |]
    let mkSigExp typ = (SigE arbi (AppT (ConT ''Gen) typ))
    let foldf e1 = UInfixE e1 ap
    let arbExps = foldl1 foldf $ map (mkSigExp . snd) records
    let exp = UInfixE ((ConE . mkName . showName) name) op arbExps
    [d| instance Arbitrary $( conT name ) where
            arbitrary = $( return exp )
      |]


-- | And one for Texts:
instance Arbitrary T.Text where
    arbitrary = T.pack <$> (arbitrary :: Gen String)


-- | A naive Arbitrary instance for Aeson.Value:
instance Arbitrary Aeson.Value where
  arbitrary = frequency [
    -- 60% of times a simple type:
    (3, simpleTypes),

    -- 20% an array:
    (1, arrayTypes),

    -- 20% an object:
    (1, objectTypes)
    ]
    where
      -- the basic types and their respective ratios:
      simpleTypes :: Gen Aeson.Value
      simpleTypes = frequency [
        (1, return Aeson.Null),
        (2, liftM Aeson.Bool (arbitrary :: Gen Bool)),
        (2, liftM (Aeson.Number . fromIntegral) (arbitrary :: Gen Int)),
        (2, liftM (Aeson.String . T.pack) (arbitrary :: Gen String))
        ]

      sizedArray n = liftM (Aeson.Array . V.fromList) $ replicateM n simpleTypes
      arrayTypes = sized sizedArray

      -- NOTE: the below values DO preserve the aforementioned ratios:
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]

      mapF (k, v) = (T.pack k, v)

      sizedObject n = liftM (Aeson.object . map mapF) $ replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
      objectTypes :: Gen Aeson.Value
      objectTypes = sized sizedObject

-- | prop_aeson is a simple propery testing Aeson's symmetry:
prop_aeson :: Aeson.Value -> Bool
prop_aeson v = v == v'
  where (Just v') = Aeson.decode $ Aeson.encode v

-- | Some interesting collects to be ran in conjunction with the above:
colNull v = collect (isNull v)      -- tells % of Nulls
colSimple v = collect (isSimple v)  -- tells % of Simple types
colArray v = collect (isArr v)      -- tells % of Arrays
colObj v = collect (isObj v)        -- tells % of Objects


isNull Aeson.Null = True
isNull _ = False

isArr (Aeson.Array _) = True
isArr _ = False

isObj (Aeson.Object _) = True
isObj _ = False

isSimple v = not $ isArr v || isObj v

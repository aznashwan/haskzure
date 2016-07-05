{-|
 Module      : Cloud.Haskzure.Gen.Utils
 Description : Template Haskell utilities for instance generation.
 Copyright   : (c) Nashwan Azhari, 2016
 License     : Apache 2.0
 Maintainer  : aznashwan@yahoo.com
 Stability   : experimental
 Portability : POSIX, Win32

 This module defines various utilities for instance declaration generation.
-}

{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cloud.Haskzure.Gen.Utils (
    mkToJSONPairs,
    mkFromJSONPairs,
    mkEncodingOptions,
    recordFieldsInfo,
    mkDecodingOptions,
    mkAzureResourceDecs,
    withDefaults
    ) where


import           Control.Applicative               (empty)
import           Data.Char                         (toLower, toUpper)
import           Data.Maybe                        (fromJust, isJust)
import           Data.Monoid                       ((<>))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax        (mkName, showName)
#if MIN_VERSION_template_haskell(2,11,0)
import           Language.Haskell.TH.Syntax        (VarBangType)
#else
import           Language.Haskell.TH.Syntax        (VarStrictType)
#endif

import           Data.Aeson                        (Value (..), object, (.=))
import           Data.Aeson.Types                  (Options (..), Pair, Parser,
                                                    defaultTaggedObject)

import           Cloud.Haskzure.Core.AzureResource (AzureResource (..))

-- | Example data used for reference:
--
-- @
-- data TestData = TestData {
--     testDataField1 :: String,
--     testDataField2 :: Int
--   } deriving (Show, Generic)
-- @

-- | Makes the list of 'KeyValue's for the provided type's FromJSON defaults.
-- It reifies the type provided by its 'Name', inspects all of its fields and
-- builds the result based on it.
-- ex: mkFromJSONPairs 'TestData -> ["field1" .= (mempty :: String) ...]
mkFromJSONPairs :: Name -> Q Exp
mkFromJSONPairs typ = do
    op <- [| (.=) |]
    records <- recordFieldsInfo (\(n,_,t) -> (n,t)) typ

    memptys <- mapM (getMemptyExp . snd) records
    let labels = map ((mkFieldLabelPrefixRemoveModifier typ) . showName . fst) records
    let zipf f m = InfixE (Just (LitE (StringL f))) op (Just m)

    return $ ListE $ zipWith zipf labels memptys

-- | Returns the 'Exp' specific to the usage of 'mempty'.
-- Ex: getMemptyExp "Type" -> (mempty :: Type)
getMemptyExp :: Type -> Q Exp
getMemptyExp t = do
    op <- [| mempty |]
    return $ SigE op t

-- | Returns the 'Exp' associated to an Aeson ToJSON .= operation for the 'Name'
-- of a given record field.
mkToJSONexp :: String -> Name -> Q Exp
mkToJSONexp jsonName vName = do
    op <- [| (.=) |]
    return $ InfixE
                (Just (LitE (StringL jsonName)))
                op
                (Just (VarE vName))

-- | Returns the list of 'Exp's associated to all the record fields for the
-- given type's SINGLE value constructor's usage with Aeson's '.='.
mkToJSONPairs :: Name -> Q Exp
mkToJSONPairs typ = do
    records <- recordFieldsInfo (\(n,_,_) -> showName n) typ
    decs <- mapM (\r -> mkToJSONexp r (mkName r)) records
    return $ ListE decs

-- | 'reify's the simple type given by 'Name' and returns the result of applying
-- the given 'VarTypeBang' (or 'VarStrictType' in template-haskell <= 2.11.0)
-- -applicable function to all the found records. This function makes hard presumptions
-- about the provided type 'Name'. Particularly, it expects it to be a datatype
-- with a single value constructor which is of record type.
#if MIN_VERSION_template_haskell(2,11,0)
recordFieldsInfo :: (VarBangType -> a) -> Name -> Q [a]
recordFieldsInfo f name = do
    -- additional (Maybe Kind) in DataD (4th position):
    TyConI (DataD _ _ _ _ [RecC _ vbts] _) <- reify name
    return $ map f vbts
#else
-- 'VarBangType' used to be called 'VarStrictType'.
recordFieldsInfo :: (VarStrictType -> a) -> Name -> Q [a]
recordFieldsInfo f name = do
    TyConI (DataD _ _ _ [RecC _ vbts] _) <- reify name
    return $ map f vbts
#endif

-- | Makes set of Aeson 'Options' for encoding datatypes.
mkEncodingOptions :: Name -> Options
mkEncodingOptions n = baseAesonOptions {
    fieldLabelModifier = mkFieldLabelPrefixRemoveModifier n
}

-- | Makes set of Aeson 'Options' for decoding datatypes.
mkDecodingOptions :: Name -> Options
mkDecodingOptions n = baseAesonOptions {
    fieldLabelModifier = mkRecordLabelPrefixAddModifier n
}

-- | Makes field label modifier by removing datatype prefix and uncapitalizing.
-- ex: TypeName "typeNameField" -> "field"
mkFieldLabelPrefixRemoveModifier :: Name -> String -> String
mkFieldLabelPrefixRemoveModifier n = lowerFirst . drop (length $ showName n)

-- | Makes record name modifier by capitalizing and adding datatype prefix.
-- ex: TypeName "field" -> "typeNameField"
mkRecordLabelPrefixAddModifier :: Name -> String -> String
mkRecordLabelPrefixAddModifier n f = lowerFirst (showName n) ++ upperFirst f
    where upperFirst (x:xs) = toUpper x : xs
          upperFirst [] = []

-- | Lowers first character in standard string.
lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

-- | looks up the given 'Name' or fails descriptively.
lookupName :: Name -> Q Name
lookupName name = (lookupValueName (showName name)) >>= \n ->
                    case n of
                      Just na -> return na
                      Nothing -> fail $ "Unable to find name: " ++ (showName name)

-- | Base Aeson Options used for encoding/decoding.
baseAesonOptions :: Options
baseAesonOptions =  Options {
    fieldLabelModifier = id,
    constructorTagModifier = id,
    allNullaryToStringTag = True,
    omitNothingFields = True,
    sumEncoding = defaultTaggedObject,
    unwrapUnaryRecords = False
}

-- | Merges two Aeson 'Value's via 'Map' 'Monoid' instance.
(<+>) :: Value -> Value -> Value
Object x <+> Object y = Object (x <> y)
_ <+> _ = error "<+>: merging non-objects"

-- | Returns new parsing function which will use the specified 'Pair's as
-- default values for the data type being decoded's fields.
withDefaults :: (Value -> Parser a) -> [Pair] -> Value -> Parser a
withDefaults parser defs js@(Object _) = parser (js <+> object defs)
withDefaults _ _ _ = empty

-- (a -> m b) -> t a -> m (t b)
-- ((Name, Name) -> Q [(Name, Name)]) -> [(Name, Name)] -> Q [(Name, Name)]

-- | Creates the '[Dec]' for an 'AzureResource' instance declaration.
mkAzureResourceDecs :: Name -> Q [Dec]
mkAzureResourceDecs name = do
    ps <- mkAzureResourceFieldPairs name
    azrs <- mapM (lookupName . fst) ps
    let pairs = zip azrs (map snd ps)

    return $ map (\(azr, r) -> ValD (VarP azr) (NormalB (VarE r)) []) pairs

-- | takes the name of the datastructure which is needed to be instantiated to
-- 'AzureResource' and returns the list of pairs of 'Name's representing the
-- AzureResource fields with their corresponding record field.
-- ex: SomeData -> [(rID, someDataID) ...]
mkAzureResourceFieldPairs :: Name -> Q [(Name, Name)]
mkAzureResourceFieldPairs name = do
    pref <- azureResourceFieldPrefix
    fields <- azureResourceFieldNames

    let adtfields =  map (\f -> ((lowerFirst . showName) (getBaseName name)) ++ (drop (length pref) f)) $ map showName fields
    return $ zip fields $ map mkName adtfields


-- | Some.Namespaced.Name -> Name
getBaseNameStr :: String -> String
getBaseNameStr = foldl (\name c -> if c == '.' then "" else name ++ [c]) ""

-- | 'getBaseStr' applied to 'Name's.
getBaseName :: Name -> Name
getBaseName = mkName . getBaseNameStr . showName

-- | The common prefix of all of the 'AzureResource' fields.
azureResourceFieldPrefix :: Q String
azureResourceFieldPrefix =
    do
        names <- fmap (map showName) azureResourceFieldNames
        case names of
          [] -> return ""
          ns -> let nhds = map heads ns in
                    return $ lastThat (\pref -> all (\nhd -> elem pref nhd) nhds) (nhds !! 1)
  where heads = foldl (\ls x -> ls ++ [last ls ++ [x]]) [[]]
        lastThat f = foldl (\c x -> if f x then x else c) ""


-- | The names of the fields of the 'AzureResource' typeclass.
azureResourceFieldNames :: Q [Name]
azureResourceFieldNames = typeClassFieldNames ''AzureResource


-- | 'typeClassFuncs' extracts the 'Name's of both the functions and values
-- declared in the typeclass with the given 'Name'.
typeClassFieldNames :: Name -> Q [Name]
typeClassFieldNames cls =
    reify cls >>= \c -> case c of
        ClassI (ClassD _ _ _ _ decs) _ -> return $ ((map getBaseName) . getNames) decs
        _ -> fail $ showName cls ++ " is not a class."

   where getName (SigD n _) = Just n
         getName _ = Nothing
         getNames = (map fromJust) . (filter isJust) . (map getName)

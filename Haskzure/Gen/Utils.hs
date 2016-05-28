{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskzure.Gen.Utils (
    mkToJSONPairs,
    mkFromJSONPairs,
    mkEncodingOptions,
    mkDecodingOptions,
    withDefaults
    ) where


import           Control.Applicative        (empty)
import           Data.Char                  (toLower, toUpper)
import           Data.Monoid                ((<>))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (VarBangType, mkName, showName)

import           Data.Aeson                 (Value (..), object)
import           Data.Aeson.Types           (Options (..), Pair, Parser,
                                             defaultTaggedObject)

-- | Example data used for reference:
-- data TestData = TestData {
--     testDataField1 :: String,
--     testDataField2 :: Int
--   } deriving (Show, Generic)

-- | Makes the list of 'KeyValue's for the provided type's FromJSON defaults.
-- It reifies the type provided by its 'Name', inspects all of its fields and
-- builds the result based on it.
-- ex: mkFromJSONPairs 'TestData -> ["field1" .= (mempty :: String) ...]
mkFromJSONPairs :: Name -> Q Exp
mkFromJSONPairs typ = do
    -- Just op <- lookupValueName ".="
    records <- recordFieldsInfo (\(n,_,t) -> (n,t)) typ

    memptys <- mapM (getMemptyExp . snd) records
    let labels = map ((mkFieldLabelPrefixRemoveModifier typ) . showName . fst) records
    let zipf f m = InfixE (Just (LitE (StringL f))) (VarE (mkName ".=")) (Just m)

    return $ ListE $ zipWith zipf labels memptys

-- | Returns the 'Exp' specific to the usage of 'mempty'.
-- Ex: getMemptyExp "Type" -> (mempty :: Type)
getMemptyExp :: Type -> Q Exp
getMemptyExp t = do
    Just op <- lookupValueName "mempty"
    return $ SigE (VarE op) t

-- | Returns the 'Exp' associated to an Aeson ToJSON .= operation for the 'Name'
-- of a given record field.
mkToJSONexp :: String -> Name -> Q Exp
mkToJSONexp jsonName vName = do
    Just op <- lookupValueName ".="
    return $ InfixE
                (Just (LitE (StringL jsonName)))
                (VarE op)
                (Just (VarE vName))

-- | Returns the list of 'Exp's associated to all the record fields for the
-- given type's SINGLE value constructor's usage with Aeson's '.='.
mkToJSONPairs :: Name -> Q Exp
mkToJSONPairs typ = do
    records <- recordFieldsInfo (\(n,_,_) -> showName n) typ
    decs <- mapM (\r -> mkToJSONexp r (mkName r)) records
    return $ ListE decs

-- | Rifies the simple type given by 'Name' and returns the result of applying
-- the given 'VarTypeBang'-applyable function to all the found records.
-- This function makes hard presumptions about the provided type Name.
-- Particularly, it expects it to be a datatype with a single value constructor
-- which is of record type.
recordFieldsInfo :: (VarBangType -> a) -> Name -> Q [a]
recordFieldsInfo f name = do
#if __GLASGOW_HASKELL__ >= 800
    TyConI (DataD _ _ _ _ [RecC _ vbts] _) <- reify name
#else
    TyConI (DataD _ _ _ [RecC _ vbts] _) <- reify name
#endif

    return $ map f vbts

-- | Makes set of Aeson 'Options' for encoding datatypes.
mkEncodingOptions :: Name -> Options
mkEncodingOptions n = aesonOptions {
    fieldLabelModifier = mkFieldLabelPrefixRemoveModifier n
}

-- | Makes set of Aeson 'Options' for decofing datatypes.
mkDecodingOptions :: Name -> Options
mkDecodingOptions n = aesonOptions {
    fieldLabelModifier = mkRecordLabelPrefixAddModifier n
}

-- | Makes field label modifier by removing datatype prefix and uncapitalizing.
-- ex: TypeName "typeNameField" -> "field"
mkFieldLabelPrefixRemoveModifier:: Name -> String -> String
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

-- | Base Aeson Options used for encoding/decoding.
aesonOptions :: Options
aesonOptions =  Options {
    fieldLabelModifier = id,
    constructorTagModifier = id,
    allNullaryToStringTag = True,
    omitNothingFields = True,
    sumEncoding = defaultTaggedObject,
    unwrapUnaryRecords = False
}

-- | Merges two Aeson values via Map Monoid instance.
(<+>) :: Value -> Value -> Value
Object x <+> Object y = Object (x <> y)
_ <+> _ = error "<+>: merging non-objects"

-- | Returns new Parser which specifies default values for object's JSON fields.
withDefaults :: (Value -> Parser a) -> [Pair] -> Value -> Parser a
withDefaults parser defs js@(Object _) = parser (js <+> object defs)
withDefaults _ _ _ = empty


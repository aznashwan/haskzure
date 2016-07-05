{-# LANGUAGE OverloadedStrings #-}


module Cloud.Haskzure.Gen.Swagger where


import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy    as HM

import           Data.Aeson           (Object, Value (..), decode)


readSwagger :: FilePath -> IO Value
readSwagger path = do
    contents <- BSL.readFile path

    let s = (decode :: BSL.ByteString -> Maybe Value) contents
    case s of
      Just v -> return v
      _ -> fail "Failed to decode Swagger."


data Definition = Definition {
    name       :: String,
    properties :: [Object]
 } deriving (Eq, Show)

unwrapObject :: Value -> Object
unwrapObject (Object m) = m
unwrapObject _ = error "Unable to unwrap object."

getDefs :: Value -> Maybe Object
getDefs (Object m) = fmap unwrapObject $ HM.lookup "definitions" m
getDefs _ = Nothing


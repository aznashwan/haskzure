{-|
 Module      : Cloud.Haskzure.Core.Auth
 Description : Data structures and functions used for authentication.
 Copyright   : (c) Nashwan Azhari, 2016
 License     : Apache 2.0
 Maintainer  : aznashwan@yahoo.com
 Stability   : experimental
 Portability : POSIX, Win32

 This modules defines a set of data structures and utilities for authenticating against Azure and fetching access tokens.
-}

{-# LANGUAGE OverloadedStrings #-}

module Cloud.Haskzure.Core.Auth (
    Credentials(..),
    Token(..),
    getToken
    ) where


import           Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as BSL
import           Text.Printf                  (printf)

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               Value (..), eitherDecode, encode,
                                               (.:))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Cloud.Haskzure.Core.Resource (Resource (..))
import           Cloud.Haskzure.Core.Utils    ()



-- | Azure's management URL.
managementUrl :: String
managementUrl = "https://management.azure.com"

-- | makes a request URL from the provided path.
mkRequestUrl :: String -> String
mkRequestUrl path = managementUrl ++ "/" ++ path

-- | Azure's API version to be used throughout the project.
apiVersion :: String
apiVersion = "2016-03-30"

setQueryStringAPIVersion :: String -> Request -> Request
setQueryStringAPIVersion v = setQueryString [("api-version", Just $ BS.pack apiVersion)]

-- | formats with subscription ID, resource group, resource type.
resourceTypePathFormat :: String
resourceTypePathFormat = "/subscriptions/%s/resourceGroups/%s/providers/%s"

-- | formats 'resourceTypePathFormat' provided the Subscription ID and 'Resource'.
mkResourceTypePath :: String -> Resource a -> String
mkResourceTypePath subId res = printf resourceTypePathFormat
                                    subId (resGroup res) (resType res)

-- | formats with subscription ID, resource group, resource type and resource name.
resourcePathFormat :: String
resourcePathFormat =  resourceTypePathFormat ++ "/%s"

mkResourcePath :: String -> Resource a -> String
mkResourcePath subId res = printf ((mkResourceTypePath subId res) ++ "/%s") (resName res)


-- | The standard set of credentials required for Azure authentication.
data Credentials = Credentials {
    tenantId       :: BS.ByteString,
    -- ^ the ID of the AD tenant within which the application is registered.

    clientId       :: BS.ByteString,
    -- ^ the client ID of the application as configured inside the Azure AD.

    subscriptionId :: BS.ByteString,
    -- ^ the ID of the subscription to be used.

    clientSecret   :: BS.ByteString
    -- ^ the client secret (aka one of the application's key).
    } deriving Show


-- | The datatype representing an Azure API token.
data Token = Token {
    token     :: BS.ByteString,
    -- ^ the 'String' representation of the API token.

    expiresOn :: Integer,
    -- ^ the 'Integer' representing the absolute epoch time moment in
    -- which the token expires.

    tokenType :: BS.ByteString
    -- ^ the type of the token; shall be "Bearer" for all intents and purposes.
    } deriving Show


instance FromJSON Token where
    parseJSON (Object o) = Token <$>
                           o .: "access_token" <*>
                           ((o .: "expires_on") >>=
                               (return . (read :: String -> Integer))) <*>
                           o .: "token_type"
    parseJSON _ = fail "Token must be deserialized from a JSON object."


mkTokenQueryParam :: Token -> QueryParam
mkTokenQueryParam t = ("Authorization", (tokenType t) `BS.append` " " `BS.append`(token t))


-- | The endpoint where all authentication requests will be made:
mkAuthEndpoint :: BS.ByteString -> BS.ByteString
mkAuthEndpoint ten = Prelude.foldr1 BS.append [
    "https://login.microsoftonline.com/",
    ten,
    "/oauth2/token"
 ]

-- a tuple representing the key and value of a query parameter.
type QueryParam = (BS.ByteString, BS.ByteString)

-- 'QueryParams' is just an alias for the key-value pairs which will be sent as
-- part of the UEB request for the API token.
type QueryParams = [QueryParam]


-- | Makes the set of 'QueryParams' based on the provided credentials.
mkTokenRequestParams :: Credentials -> QueryParams
mkTokenRequestParams creds = [
    ("resource", "https://management.core.windows.net/"),
    ("grant_type", "client_credentials"),
    ("client_id", clientId creds),
    ("client_secret", clientSecret creds)
 ]


-- | Requests and deserializes an API 'Token'.
getToken :: Credentials -> IO Token
getToken creds = do
    request <- parseUrl (BS.unpack $ (mkAuthEndpoint $ tenantId creds)) >>=
        (return . urlEncodedBody (mkTokenRequestParams creds))

    manager <- newManager tlsManagerSettings

    resp <- httpLbs request manager

    let token = (eitherDecode :: BSL.ByteString -> Either String Token) $ responseBody resp

    case token of
      Left m -> fail m
      Right t -> return t

-- | creates or updates an Azure 'Resource' by issuing a PUT request on the
-- appropriate URL using the given credentials.
createOrUpdate :: (ToJSON a) => Credentials -> Resource a -> IO ()
createOrUpdate creds res = do
    token <- getToken creds

    req <- parseUrl $ mkResourcePath (BS.unpack $ subscriptionId creds) res
    let req = req {
        method = "PUT",
        requestBody = RequestBodyLBS $ encode $ res
        }
    let req = setQueryStringAPIVersion apiVersion req

    manager <- newManager tlsManagerSettings

    resp <- httpLbs req manager

    print $ responseBody resp



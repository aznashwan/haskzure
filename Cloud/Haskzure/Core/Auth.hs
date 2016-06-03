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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Cloud.Haskzure.Core.Auth (
    Credentials(..),
    Token(..),
    getToken
    ) where


import qualified Data.ByteString           as BS
import           Data.ByteString.Char8     ()
import qualified Data.ByteString.Lazy      as BSL

import           Data.Aeson                (FromJSON (..), Value (..),
                                            eitherDecode, (.:))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Cloud.Haskzure.Core.Utils ()



-- | The standard set of credentials required for Azure authentication.
data Credentials = Credentials {
    tenantId     :: BS.ByteString,
    -- ^ the ID of the AD tenant within which the application is registered.

    clientId     :: BS.ByteString,
    -- ^ the client ID of the application as configured inside the Azure AD.

    clientSecret :: BS.ByteString
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



-- | The endpoint where all authentication requests will be made:
mkAuthEndpoint :: BS.ByteString -> BS.ByteString
mkAuthEndpoint ten = foldr1 BS.append [
    "https://login.microsoftonline.com/",
    ten,
    "/oauth2/token"
 ]


-- 'QueryParams' is just an alias for the key-value pairs which will be sent as
-- part of the UEB request for the API token.
type QueryParams = [(BS.ByteString, BS.ByteString)]


-- | Makes the set of 'QueryParams' based on the provided credentials.
mkTokenRequestParams :: Credentials -> QueryParams
mkTokenRequestParams creds = [
    ("resource", "https://management.core.windows.net/"),
    ("grant_type", "client_credentials"),
    ("client_id", clientId creds),
    ("client_secret", clientSecret creds)
 ]


-- | Requests and deserializes an API 'Token'.
getToken :: Credentials -> IO (Either String Token)
getToken creds = do
    request <- parseUrl (filter (/='\"') $ show $ (mkAuthEndpoint $ tenantId creds)) >>=
        (return . urlEncodedBody (mkTokenRequestParams creds))

    manager <- newManager tlsManagerSettings

    resp <- httpLbs request manager

    BSL.writeFile "./resp.json" $ responseBody resp

    return $ (eitherDecode :: BSL.ByteString -> Either String Token)
            $ responseBody resp

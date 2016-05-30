{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Cloud.Haskzure.Core.Auth where


import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL

import           Data.Aeson                (FromJSON (..), Value (..),
                                            eitherDecode, (.:))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Cloud.Haskzure.Core.Utils ()



-- | The standard set of credentials required for Azure authentication.
data Credentials = Credentials {
    username :: BS.ByteString,
    -- ^ the Azure username of the form 'someuser@someapp.onmicrosoft.com'

    password :: BS.ByteString
    -- ^ the password of the said user.
    } deriving Show


-- | The datatype representing an Azure API token.
data Token = Token {
    token        :: BS.ByteString,
    -- ^ the 'BS.ByteString' representation of the API token.

    refreshToken :: BS.ByteString,
    -- ^ the token to be used for directly refreshing the API token.

    expiresOn    :: Integer,
    -- ^ the 'Integer' representing the absolute from epoch time moment in
    -- which the token expires.

    tokenType    :: BS.ByteString
    -- ^ the type of the token; shall be "Bearer" for all intents and purposes.
    } deriving Show


instance FromJSON Token where
    parseJSON (Object o) = Token <$>
                           o .: "access_token" <*>
                           o .: "refresh_token" <*>
                           ((o .: "expires_on") >>= (return . (read :: String -> Integer))) <*>
                           o .: "token_type"
    parseJSON _ = fail "Token must be deserialized from a JSON object."


-- | The endpoint where all authentication requests will be made:
auth_endpoint :: String
auth_endpoint = "http://login.microsoftonline.com/common/oauth2/token"


type QueryParams = [(BS.ByteString, BS.ByteString)]

-- | The standard query parameters:
azureQueryParams :: QueryParams
azureQueryParams = [
    ("grant_type", "password"),
    ("resource", "https://management.core.windows.net/")
    -- ("client_id", "hopefully unneeded"),
 ]


-- | Adds credentials to query params:
addCredentials :: Credentials -> QueryParams -> QueryParams
addCredentials (Credentials user pass) = (++) [("username", user), ("password", pass)]


getToken :: Credentials -> IO (Either String Token)
getToken creds = do
    manager <- newManager tlsManagerSettings

    request <- parseUrl auth_endpoint >>=
        (return . urlEncodedBody (addCredentials creds azureQueryParams))

    resp <- httpLbs request manager

    return $ (eitherDecode :: BSL.ByteString -> Either String Token) $ responseBody resp

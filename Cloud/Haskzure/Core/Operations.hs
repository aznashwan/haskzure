{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cloud.Haskzure.Core.Operations (
    createOrUpdate,
    get
    ) where


import           Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy         as BSL
import           Data.Maybe                   (fromJust)
import           Text.Printf                  (printf)

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               decode, encode)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types           (Header, Method, hAuthorization)

import           Cloud.Haskzure.Core.Auth     (Credentials (..), Token (..),
                                               getToken)
import           Cloud.Haskzure.Core.Resource (Resource (..))
import           Cloud.Haskzure.Core.Utils    ()


-- | Azure's management URL.
managementUrl :: String
managementUrl = "https://management.azure.com"

-- | makes a request URL from the provided path.
mkRequestUrl :: String -> String
mkRequestUrl = (++) (managementUrl ++ "/")

-- | Azure's API version to be used throughout the project.
apiVersion :: String
apiVersion = "2016-03-30"

setQueryStringAPIVersion :: String -> Request -> Request
setQueryStringAPIVersion v = setQueryString [("api-version", Just $ BS.pack v)]

-- | formats with subscription ID, resource group, resource type.
resourceTypePathFormat :: String
resourceTypePathFormat = "/subscriptions/%s/resourceGroups/%s/providers/%s"

-- | formats with subscription ID, resource group, resource type and resource name.
resourcePathFormat :: String
resourcePathFormat =  resourceTypePathFormat ++ "/%s"

-- | formats 'resourceTypePathFormat' provided the Subscription ID and 'Resource'.
mkResourceTypePath :: String -> Resource a -> String
mkResourceTypePath subId res = printf resourcePathFormat
                                    subId (resGroup res) (resType res) (resName res)

mkResourcePath :: String -> Resource a -> String
mkResourcePath subId res = printf ((mkResourceTypePath subId res) ++ "/%s") (resName res)

mkTokenHeader :: Token -> Header
mkTokenHeader t = (hAuthorization, (tokenType t) `BS.append` " " `BS.append` (token t))

setTokenRequestHeader :: Token -> Request -> Request
setTokenRequestHeader t req = req { requestHeaders = [mkTokenHeader t] }

mkRequestForResource :: Credentials -> (Resource a) -> IO Request
mkRequestForResource creds = parseUrl . mkRequestUrl . (mkResourcePath (BS.unpack $ subscriptionId creds))

prepareRequest :: Method -> Token -> Request -> Request
prepareRequest m tk = (\req -> req { method = m }) . (setTokenRequestHeader tk) . (setQueryStringAPIVersion apiVersion)

preparePOSTRequest :: Token -> Request -> Request
preparePOSTRequest = prepareRequest "POST"

prepareGETRequest :: Token -> Request -> Request
prepareGETRequest = prepareRequest "GET"

setRequestBodyBSL :: BSL.ByteString -> Request -> Request
setRequestBodyBSL b r = r { requestBody = RequestBodyLBS b }


-- | creates or updates an Azure 'Resource' by issuing a PUT request on the
-- appropriate URL using the given 'Credentials'.
createOrUpdate :: (ToJSON a) => Credentials -> Resource a -> IO ()
createOrUpdate creds res = do
    tk <- getToken creds

    req <- fmap ((setRequestBodyBSL $ encode res) . (preparePOSTRequest tk))  (mkRequestForResource creds res)

    manager <- newManager tlsManagerSettings

    resp <- httpLbs req manager

    print $ responseBody resp


-- | gets the given Azure 'Resource' by issuing a GET request on the
-- appropriate URL using the given 'Credentials'.
get :: (FromJSON a) => Credentials -> Resource a -> IO (Resource a)
get creds res = do
    tk <- getToken creds

    req <- fmap (prepareGETRequest tk) (mkRequestForResource creds res)

    manager <- newManager tlsManagerSettings

    resp <- httpLbs req manager

    print $ responseBody resp

    return $ fromJust . decode $ responseBody resp

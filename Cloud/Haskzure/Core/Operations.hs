{-# LANGUAGE OverloadedStrings #-}

module Cloud.Haskzure.Core.Operations (
    createOrUpdate
    ) where


import           Data.ByteString.Char8        as BS
import           Text.Printf                  (printf)

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               encode)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header

import           Cloud.Haskzure.Core.Auth     (Credentials (..), Token (..),
                                               getToken)
import           Cloud.Haskzure.Core.Resource (Resource (..))
import           Cloud.Haskzure.Core.Utils    ()


-- | Azure's management URL.
managementUrl :: String
managementUrl = "https://management.azure.com"

-- | makes a request URL from the provided path.
mkRequestUrl :: String -> String
mkRequestUrl p = managementUrl ++ "/" ++ p

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
mkTokenHeader t = (hAuthorization, (tokenType t) `BS.append` " " `BS.append`(token t))

setTokenRequestHeader :: Token -> Request -> Request
setTokenRequestHeader t req = req { requestHeaders = [mkTokenHeader t] }

-- | creates or updates an Azure 'Resource' by issuing a PUT request on the
-- appropriate URL using the given credentials.
createOrUpdate :: (ToJSON a) => Credentials -> Resource a -> IO ()
createOrUpdate creds res = do
    tk <- getToken creds

    r <- parseUrl $ mkRequestUrl $ mkResourcePath (BS.unpack $ subscriptionId creds) res
    let req = (\req -> req {
        method = "PUT",
        requestBody = RequestBodyLBS $ encode $ res
        }) . (setTokenRequestHeader tk) . (setQueryStringAPIVersion apiVersion) $ r

    manager <- newManager tlsManagerSettings

    resp <- httpLbs req manager

    print $ responseBody resp

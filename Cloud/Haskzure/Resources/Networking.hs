{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cloud.Haskzure.Resources.Networking where


import qualified Data.ByteString.Char8        as BS

import           GHC.Generics                 (Generic)

import           Cloud.Haskzure.Core.Utils    ()
import           Cloud.Haskzure.Gen.Instances (mkAllInsts)


data AddressSpace = AddressSpace {
        addressSpaceAddressPrefixes :: [BS.ByteString]
    } deriving (Generic, Show, Eq)
mkAllInsts ''AddressSpace

data DhcpOptions = DhcpOptions {
        dhcpOptionsDnsServers :: [BS.ByteString]
    } deriving (Generic, Show, Eq)
mkAllInsts ''DhcpOptions

data Subnet = Subnet {
        subnetName          :: BS.ByteString,
        subnetAddressPrefix :: BS.ByteString
        -- subnetIpConfigurations :: [IPConfiguration] staging issue...
    } deriving (Generic, Show, Eq)
mkAllInsts ''Subnet

data IPConfiguration = IPConfiguration {
        ipConfigurationPrivateIPAddress          :: BS.ByteString,
        -- TODO: Enum:
        ipConfigurationPrivateIPAllocationMethod :: BS.ByteString,
        ipConfigurationSubnet                    :: Subnet
    } deriving (Generic, Show, Eq)
mkAllInsts ''IPConfiguration

data PublicIPAddress = PublicIPAddress {
        -- TODO: Enum:
        publicIPAddressPublicIPAllocationMethod :: BS.ByteString,
        publicIPIpConfiguration                 :: IPConfiguration
    } deriving (Generic, Show, Eq)
mkAllInsts ''PublicIPAddress

data VN = VN {
        vnAddressSpace :: AddressSpace,
        vnDhcpOptions  :: DhcpOptions,
        vnSubnets      :: [Subnet]
    } deriving (Generic, Show, Eq)
mkAllInsts ''VN


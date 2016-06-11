module Haskzure.VirtualMachines.VirtualMachine where


data VirtualMachineProperties = VirtualMachineProperties {
    hardwareProfile   :: HardwareProfile,
    storageProfile    :: StorageProfile,
    osProfile         :: OSProfile,
    networkProfile    :: NetworkProfile,
    provisioningState :: String,
    vmID              :: String
}

data HardwareProfile = HardwareProfile {
    vmSize         :: String,
    imageReference :: ImageReference,
    osDisk         :: OSDisk,
    dataDisks      :: [DataDisk]
}

data ImageReference = ImageReference {
    publisher :: String,
    offer     :: String,
    sku       :: String,
    version   :: String
}

data OSDisk = OSDisk {
    osType       :: String,
    name         :: String,
    vhd          :: VirtualHardDisk,
    image        :: VirtualHardDisk,
    caching      :: String,
    diskSizeGb   :: Int,
    createOption :: String
}

data StorageProfile = StorageProfile {
    imageReference :: ImageReference
}


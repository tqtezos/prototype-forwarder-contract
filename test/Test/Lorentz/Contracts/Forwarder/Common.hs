module Test.Lorentz.Contracts.Forwarder.Common
  ( centralWallet
  , masterAddress
  ) where


import Lorentz

import Lorentz.Test

-- | By default, we set master and admin to this address.
masterAddress :: Address
masterAddress = genesisAddress

centralWallet :: Address
centralWallet = genesisAddress2

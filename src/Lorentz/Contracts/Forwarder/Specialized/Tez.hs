{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wall -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Forwarder.Specialized.Tez where

import Lorentz

import Prelude (Enum(..), Eq(..), ($), String, show)



type Parameter = ()

type Storage = ()

specializedTezForwarderContract :: Address -> ContractCode Parameter Storage
specializedTezForwarderContract centralWallet = do
  cdr
  nil
  push centralWallet
  contract @()
  ifNone
    failWith
    (do
      balance
      unit
      transferTokens @()
      cons
      pair
    )


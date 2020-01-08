{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lorentz.Contracts.Validate.Reception where

import Prelude hiding ((>>), drop)
import GHC.Generics (Generic)
import Text.Show (Show(..))
-- import Data.Functor.Classes
import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec (look)

import qualified Data.Set as Set (fromList)

import Lorentz
import Michelson.Text
import Michelson.Typed.EntryPoints
import Michelson.Typed.Scope
-- import Tezos.Address

import Lorentz.Contracts.View
import Lorentz.Contracts.DS.V1.Registry.Types (InvestorId(..))

instance Read MText where
  readPrec = do
    eMText <- mkMText . fromString <$> look
    case eMText of
      Left err -> fail $ show err
      Right mText' -> return mText'

deriving instance Read InvestorId

-- | A `Set` of allowed senders
type Whitelist = Set InvestorId

-- | Given a `receivedAmount` and sender `InvestorId`,
-- the `validateReceptionContract` will ensure that
-- the `InvestorId` is in the `Whitelist` and
-- subtract the `receivedAmount` from the `tokenLimit`.
--
-- If the `InvestorId` is not in the `Whitelist` or
-- the `tokenLimit` is exceeded, the operation will fail.
data ReceptionParameters = ReceptionParameters
  { receivedAmount :: !Natural
  , fromUser       :: !InvestorId
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | `coerce_` from `ReceptionParameters`
unReceptionParameters :: ReceptionParameters & s :-> (Natural, InvestorId) & s
unReceptionParameters = coerce_

-- | For `Validate`, see `ReceptionParameters`.
--
-- Otherwise, offers `View_`s for its `Storage`
data Parameter
  = Validate !ReceptionParameters
  | GetDSAddress !(View_ Address)
  | GetRemaining !(View_ Natural)
  | GetWhitelist !(View_ Whitelist)
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepNone

-- | The Address of the associated DS Token contract, a `Whitelist` of allowed
-- sending users and how many tokens may be forwarded.
data Storage = Storage
  { dsTokenAddress :: !Address
  , whitelist      :: !Whitelist
  , tokenLimit     :: !Natural
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | `coerce_` from `Storage`
unStorage :: Storage & s :-> (Address, (Whitelist, Natural)) & s
unStorage = coerce_

-- | `coerce_` to `Storage`
toStorage :: (Address, (Whitelist, Natural)) & s :-> Storage & s
toStorage = coerce_

-- | Convenient `Storage` constructor
mkStorage :: Address -> [InvestorId] -> Natural -> Storage
mkStorage dsTokenAddress' whitelist' tokenLimit' =
  Storage
    dsTokenAddress'
    (Set.fromList whitelist')
    tokenLimit'

-- | Assert that the `sender` is the given DS Token address
assertDSIsCaller :: Address & s :-> Address & s
assertDSIsCaller = do
  dup
  sender
  assertEq $ mkMTextUnsafe "not DS"

-- | Assert the given `InvestorId` is in the `Whitelist`
assertInWhitelist :: InvestorId & Whitelist & s :-> Whitelist & s
assertInWhitelist = do
  dip dup
  mem
  assert $ mkMTextUnsafe "not in whitelist"

-- | Accepts @tokensToConsume, tokenLimit@ and produces a new @tokenLimit@
-- by subtracting @tokensToConsume@ and asserting the result is non-negative
consumeTokenLimit :: Natural & Natural & s :-> Natural & s
consumeTokenLimit = do
  swap
  sub
  isNat
  assertSome $ mkMTextUnsafe "token limit exceeded"

validateReceptionContract :: ()
  => Contract Parameter Storage
validateReceptionContract = do
  unpair
  caseT @Parameter
    ( #cValidate /-> do
        stackType @[ReceptionParameters, Storage]
        dip $ do
          unStorage
          unpair
          stackType @[Address, (Whitelist, Natural)]
          assertDSIsCaller
          swap
          unpair
          stackType @[Whitelist, Natural, Address]
        unReceptionParameters
        unpair
        stackType @[Natural, InvestorId, Whitelist, Natural, Address]
        dip $ do
          assertInWhitelist
          swap
        stackType @[Natural, Natural, Whitelist, Address]
        consumeTokenLimit
        stackType @[Natural, Whitelist, Address]
        swap
        pair
        swap
        pair
        toStorage
        nil
        pair
    , #cGetDSAddress /-> viewUnit_ $ do
        unStorage
        car
    , #cGetRemaining /-> viewUnit_ $ do
        unStorage
        cdr
        cdr
    , #cGetWhitelist /-> viewUnit_ $ do
        unStorage
        cdr
        car
    )


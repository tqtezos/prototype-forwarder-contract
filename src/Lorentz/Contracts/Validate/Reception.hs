{-# LANGUAGE TypeFamilies #-}
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lorentz.Contracts.Validate.Reception where

import Prelude hiding ((>>), drop)
import GHC.Generics (Generic)
import Text.Show (Show(..))
import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec (look)

import qualified Data.Set as Set (fromList)

import Lorentz
import Michelson.Text
import Michelson.Typed.EntryPoints
import Michelson.Typed.Scope

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

-- | For `Validate`, see `ReceptionParameters`.
--
-- Otherwise, offers `View_`s for its `Storage`
data Parameter
  = Validate !InvestorId -- throw error unless InvestorId in Whitelist
  | GetWhitelist !(View_ Whitelist)
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain -- None

-- | The Address of the associated DS Token contract, a `Whitelist` of allowed
-- sending users and how many tokens may be forwarded.
data Storage = Storage
  { whitelist :: !Whitelist
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | `forcedCoerce_` from `Storage`
unStorage :: Storage & s :-> Whitelist & s
unStorage = forcedCoerce_

-- | `forcedCoerce_` to `Storage`
toStorage :: Whitelist & s :-> Storage & s
toStorage = forcedCoerce_

-- | Convenient `Storage` constructor
mkStorage :: [InvestorId] -> Storage
mkStorage whitelist' =
  Storage
    (Set.fromList whitelist')

-- | Assert the given `InvestorId` is in the `Whitelist`
assertInWhitelist :: InvestorId & Whitelist & s :-> Whitelist & s
assertInWhitelist = do
  dip dup
  mem
  assert $ mkMTextUnsafe "not in whitelist"

validateReceptionContract :: ()
  => Contract Parameter Storage
validateReceptionContract = do
  unpair
  caseT @Parameter
    ( #cValidate /-> do
        stackType @[InvestorId, Storage]
        dip $ do
          unStorage
          stackType @('[Whitelist])
        stackType @[InvestorId, Whitelist]
        assertInWhitelist
        stackType @('[Whitelist])
        toStorage
        nil
        pair
    , #cGetWhitelist /-> viewUnit_ $ do
        unStorage
    )


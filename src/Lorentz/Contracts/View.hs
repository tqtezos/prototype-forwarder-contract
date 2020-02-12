{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wno-simplifiable-class-constraints #-}

module Lorentz.Contracts.View where

import Prelude hiding ((>>))
import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec (look)
import Data.Functor.Classes

import Lorentz
import Lorentz.Value
import Tezos.Address

-- | A `View` accepting `()` as its argument
type View_ = View ()

-- | Construct a `View_`
toView_ :: ToContractRef r a => a -> View_ r
toView_ = mkView ()

-- | `view_` specialized to `View_`
viewUnit_ :: NiceParameter r =>
  (forall (s0 :: [*]). (storage & s0) :-> (r : s0))
  -> (View_ r & (storage & s)) :-> ((List Operation, storage) & s)
viewUnit_ f = do
  view_ $ do
    cdr
    f

-- | Uses `parseAddress` with the remainder of the input
instance Read (TAddress r) where
  readPrec = do
    eAddress <- parseAddress . fromString <$> look
    case eAddress of
      Left err -> fail $ show err
      Right address' -> return $ toTAddress address'

-- | Uses `readBinaryWith`
instance (Read a, NiceParameter r, ToContractRef r (TAddress r)) => Read (View a r) where
  readPrec = readBinaryWith readPrec readPrec "View" $
    (\x -> View x . toContractRef @r @(TAddress r))


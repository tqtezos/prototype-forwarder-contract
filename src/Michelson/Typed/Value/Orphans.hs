{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wno-orphans #-}

module Michelson.Typed.Value.Orphans where

import Lorentz
import Michelson.Typed.Instr
import Michelson.Typed.Value
import Michelson.Typed.Annotation

import Data.Singletons (SingI)
import Prelude (id)

instance IsoValue (Value' Instr a) where
  type ToT (Value' Instr a) = a
  toVal = id
  fromVal = id

instance SingI t => HasTypeAnn (Value t) where
  getTypeAnn = starNotes


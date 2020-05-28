{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS -Wall -Wno-orphans #-}

module Main where

import System.IO hiding (putStrLn, print)
import Data.String
import GHC.Natural
import Prelude hiding (putStrLn, show, print)

import Fmt (pretty)

import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Data.Singletons
import Data.Constraint
import Data.Typeable (eqT, Typeable, (:~:)(..))

import Tezos.Address (Address(..))
import qualified Tezos.Address as Tezos
import qualified Tezos.Core as Core (parseTimestamp)
import Lorentz (Timestamp, ToT, IsoValue)
import qualified Lorentz as L
import Util.IO (hSetTranslit, writeFileUtf8)
import Universum.Print
import Universum.String
import Universum.Lifted
import Universum.Exception

import Michelson.TypeCheck
import Michelson.Typed.Instr (FullContract(..))
import Michelson.Typed.Scope
import Michelson.Typed.T
import Michelson.Untyped.Annotation (noAnn)

import Morley.Nettest (NettestClientConfig(..))

import qualified Lorentz.Contracts.Forwarder.Specialized as Specialized
import qualified Lorentz.Contracts.Forwarder.Specialized.FlushAny as Specialized
import qualified Lorentz.Contracts.Forwarder.Specialized.FlushAny.Tez as Specialized (specializedAnyTezForwarderContract)
import qualified Lorentz.Contracts.Forwarder.Specialized.FlushAny.ForwardAnyFA12 as Specialized (specializedAnyFA12ForwarderContract)
import qualified Michelson.Untyped.Type as U

deriving instance Show (L.FutureContract p)

-- | Configuration for testing the forwarder on a real network
nettestConfig :: NettestClientConfig
nettestConfig = NettestClientConfig
  { nccScenarioName = Just "Forwarder"
  , nccNodeAddress = Nothing
  , nccNodePort = Nothing
  , nccTezosClient = "tezos-client"
  , nccNodeUseHttps = False
  , nccVerbose = True
  }

-- | Parse an address argument, given its field name and description
parseAddress :: String -> String -> Opt.Parser Address
parseAddress name hInfo = Opt.option (Opt.eitherReader parseAddrDo) $ mconcat
  [ Opt.long name
  , Opt.metavar "ADDRESS"
  , Opt.help hInfo
  ]
  where
    parseAddrDo =
      either (Left . mappend "Failed to parse address: " . pretty) Right .
      Tezos.parseAddress .
      toText

-- | Parse a natural number argument, given its field name
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " ++ name ++ "."
    ]

-- | Parse a `String`
parseString :: String -> Opt.Parser String
parseString name = Opt.strOption $ mconcat
  [ Opt.long name
  , Opt.metavar "STRING"
  , Opt.help $ "String representing " ++ name ++ "."
  ]

-- | Parse a list of `String`s
parseStrings :: String -> Opt.Parser [String]
parseStrings name = Opt.option Opt.auto $ mconcat
  [ Opt.long name
  , Opt.metavar "[STRING]"
  , Opt.help $ "List of Strings representing " ++ name ++ "."
  ]

-- | Parse a `Timestamp` argument, given its field name
parseTimestamp :: String -> Opt.Parser Timestamp
parseTimestamp name =
  Opt.option (Opt.maybeReader (Core.parseTimestamp . fromString)) $
  mconcat
    [ Opt.long name
    , Opt.metavar "TIMESTAMP"
    , Opt.help $ "Timestamp number representing " ++ name ++ "."
    ]

data CmdLnArgs
  = PrintSpecializedFA12 !Address !Address !(Maybe FilePath) !Bool
  | PrintSpecializedAnyFA12 !Address !(Maybe FilePath) !Bool
  | PrintSpecializedAnyTezFA12 !Address !(Maybe FilePath) !Bool
  | PrintSpecializedForwardAnyFA12 !Address !(Maybe FilePath) !Bool
  | FlushSpecializedAnyForwarder { amountToFlush :: !Natural, tokenContract :: !Address, mOutput :: !(Maybe FilePath) }
  deriving (Show)

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ printSpecializedFA12SubCmd
  , printSpecializedAnyFA12SubCmd
  , printSpecializedAnyTezFA12SubCmd
  , printSpecializedForwardAnyFA12SubCmd
  , flushSpecializedAnyForwarderSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSpecializedFA12SubCmd =
      mkCommandParser "print-specialized-fa12"
      (PrintSpecializedFA12 <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        parseAddress "fa12-address" "Address of FA1.2 Token contract" <*>
        outputOption <*>
        onelineOption
      )
      ("Dump FA1.2 Token Forwarder contract, specialized to paricular addresses, " <>
      "in the form of Michelson code")

    printSpecializedAnyFA12SubCmd =
      mkCommandParser "print-specialized-any-fa12"
      (PrintSpecializedAnyFA12 <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        outputOption <*>
        onelineOption
      )
      ("Dump FA1.2 Token Forwarder contract for any FA1.2 token, " <>
      "specialized to paricular addresses, " <>
      "in the form of Michelson code")

    printSpecializedAnyTezFA12SubCmd =
      mkCommandParser "print-specialized-any-tez-fa12"
      (PrintSpecializedAnyTezFA12 <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        outputOption <*>
        onelineOption
      )
      ("Dump FA1.2 Token Forwarder contract for any FA1.2 token or Tez, " <>
      "specialized to paricular addresses, " <>
      "in the form of Michelson code")

    printSpecializedForwardAnyFA12SubCmd =
      mkCommandParser "print-specialized-forward-any-fa12"
      (PrintSpecializedForwardAnyFA12 <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        outputOption <*>
        onelineOption
      )
      ("Dump FA1.2 Token Forwarder contract for any FA1.2 token, " <>
      "specialized to paricular addresses, " <>
      "in the form of Michelson code")

    flushSpecializedAnyForwarderSubCmd =
      mkCommandParser "flush-specialized-any-forwarder"
      (FlushSpecializedAnyForwarder <$>
        parseNatural "tokens-to-flush" <*>
        parseAddress "tokens-contract" "Address of FA1.2 token contract" <*>
        outputOption
      )
      "Parameter to flush specialized any-token forwarder"

    outputOption = Opt.optional $ Opt.strOption $ mconcat
      [ Opt.short 'o'
      , Opt.long "output"
      , Opt.metavar "FILEPATH"
      , Opt.help "Output file"
      ]

    onelineOption = Opt.switch (
      Opt.long "oneline" <>
      Opt.help "Force single line output")


programInfo :: Opt.ParserInfo CmdLnArgs
programInfo = Opt.info (Opt.helper <*> argParser) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "CLI for Token Forwarder contract"
  , Opt.header "Token Forwarder"
  , Opt.footerDoc usageDoc
  ]

usageDoc :: Maybe Doc
usageDoc = Just $ mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  prototype-forwarder-contract print --help", linebreak
   ]

toUT :: T -> U.T
toUT (Tc ct) = U.Tc ct
toUT TKey = U.TKey
toUT TUnit = U.TUnit
toUT TChainId = U.TChainId
toUT TSignature = U.TSignature
toUT TOperation = U.TOperation
toUT (TOption t) = U.TOption $ toUT t `U.Type` noAnn
toUT (TList t) = U.TList $ toUT t `U.Type` noAnn
toUT (TSet ct) = U.TSet $ U.Comparable ct noAnn
toUT (TContract t) = U.TContract $ toUT t `U.Type` noAnn
toUT (TPair s t) = U.TPair noAnn noAnn (toUT s `U.Type` noAnn) (toUT t `U.Type` noAnn)
toUT (TOr s t) = U.TOr noAnn noAnn (toUT s `U.Type` noAnn) (toUT t `U.Type` noAnn)
toUT (TLambda s t) = U.TLambda (toUT s `U.Type` noAnn) (toUT t `U.Type` noAnn)
toUT (TMap s t) = U.TMap (U.Comparable s noAnn) (toUT t `U.Type` noAnn)
toUT (TBigMap s t) = U.TBigMap (U.Comparable s noAnn) (toUT t `U.Type` noAnn)

-- | Assert `HasNoOp`
assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

-- | Assert `HasNoContract`
assertContractTypeAbsense :: forall (t :: T) a. SingI t => (HasNoContract t => a) -> a
assertContractTypeAbsense f =
  case contractTypeAbsense  (sing @t) of
    Nothing -> error "assertContractTypeAbsense"
    Just Dict -> forbiddenContractType  @t f

-- | Assert `HasNoBigMap`
assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

-- | Assert `HasNoNestedBigMaps`
assertNestedBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapAbsense f =
  case nestedBigMapsAbsense (sing @t) of
    Nothing -> error "assertNestedBigMapAbsense"
    Just Dict -> forbiddenNestedBigMaps @t f

someLorentzContract
  :: forall param.
     (L.NiceParameterFull param, IsoValue param, Typeable param, Typeable (ToT param))
  => SomeContract -> L.SomeContract
someLorentzContract (SomeContract (contract' :: FullContract cp st)) =
  case contract' of
    FullContract{..} ->
      case eqT @(ToT param) @cp of
        Nothing -> error "The parameter type is incorrect"
        Just Refl ->
          assertOpAbsense @cp $
          assertNestedBigMapAbsense @cp $
          assertOpAbsense @st $
          assertNestedBigMapAbsense @st $
          assertContractTypeAbsense @st $
          L.SomeContract $
          (L.I fcCode :: L.ContractCode param (L.Value st))

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr
  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    printFunc
      :: (Print text, IsString text, Monoid text)
      => (FilePath -> text -> IO ())
      -> Maybe FilePath
      -> text
      -> IO ()
    printFunc writeToFile = maybe putStrLn (\file -> writeToFile file . flip mappend "\n")
    writeFunc = printFunc writeFileUtf8

    run :: CmdLnArgs -> IO ()
    run = \case
      PrintSpecializedFA12 centralWalletAddr' fa12ContractAddr' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Specialized.specializedForwarderContract centralWalletAddr' $
            fa12ContractAddr'
      PrintSpecializedAnyFA12 centralWalletAddr' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Specialized.specializedAnyForwarderContract centralWalletAddr'
      PrintSpecializedAnyTezFA12 centralWalletAddr' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Specialized.specializedAnyTezForwarderContract centralWalletAddr'
      PrintSpecializedForwardAnyFA12 centralWalletAddr' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Specialized.specializedAnyFA12ForwarderContract centralWalletAddr'
      FlushSpecializedAnyForwarder {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        Specialized.mkParameter amountToFlush tokenContract


-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS -Wall -Wno-orphans #-}

module Main where

import System.IO hiding (putStrLn, print)
import Data.String
import GHC.Natural
import Prelude hiding (putStrLn, show, print)

import Fmt (pretty, fmt, build, nameF)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import qualified Data.Map as Map
import Data.Singletons
import Data.Constraint

import Tezos.Address (Address(..), parseAddress)
import qualified Tezos.Core as Core (parseTimestamp)
import Lorentz (Timestamp)
import qualified Lorentz as L
import Michelson.Analyzer (AnalyzerRes)
import Util.IO (hSetTranslit, writeFileUtf8)
import Universum.Print
import Universum.String
import Universum.Lifted
import Universum.Exception

import Michelson.Macro
import Michelson.Runtime
import Michelson.TypeCheck
import qualified Michelson.Untyped.Type as U
import Michelson.Untyped.Annotation (noAnn)
import Michelson.Text
import Michelson.Typed.T
import Michelson.Typed.Scope
import Michelson.Typed.Instr (FullContract(..))
import qualified Lorentz.Contracts.Forwarder.Specialized as Specialized
import qualified Lorentz.Contracts.Forwarder.DS.V1.Specialized as DS
import qualified Lorentz.Contracts.Forwarder.DS.V1.ValidatedExpiring as DS
import qualified Lorentz.Contracts.Forwarder.DS.V1.ValidatedExpiring as ValidatedExpiring
import qualified Lorentz.Contracts.Forwarder.DS.V1 as DS
import qualified Lorentz.Contracts.DS.V1 as DSToken
import Lorentz.Contracts.DS.V1.Registry.Types (InvestorId(..))
import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

import Lorentz.Contracts.View
import qualified Lorentz.Contracts.Product as Product
import qualified Lorentz.Contracts.Expiring as Expiring
import qualified Lorentz.Contracts.Validate.Reception as ValidateReception

deriving instance Show (L.FutureContract p)


-- -- | Parse an `Address` argument, given its field name
-- parseAddress :: String -> Opt.Parser Address
-- parseAddress name =
--   Opt.option Opt.auto $
--   mconcat
--     [ Opt.long name
--     , Opt.metavar "ADDRESS"
--     , Opt.help $ "Address of the " ++ name ++ "."
--     ]

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
  = Print !(Maybe FilePath) !Bool
  | PrintSpecialized !Address !(L.FutureContract DSToken.Parameter) !(Maybe FilePath) !Bool
  | PrintSpecializedFA12 !Address !(L.FutureContract ManagedLedger.Parameter) !(Maybe FilePath) !Bool
  | PrintValidatedExpiring !Address !(L.FutureContract DSToken.Parameter) !(Maybe FilePath) !Bool
  | InitialStorage !Address !Address !(Maybe FilePath)
  | InitialStorageValidatedExpiring
      { dsTokenAddress :: !Address
      , whitelist :: ![String]
      , tokenLimit :: Natural
      , expirationDate :: !Timestamp
      , mOutput :: !(Maybe FilePath)
      }
  | FlushForwarder { amountToFlush :: !Natural, mOutput :: !(Maybe FilePath) }
  | ValidateTransfer { receivedAmount :: !Natural, fromUser :: !InvestorId, mOutput :: !(Maybe FilePath) }
  | GetExpiration { callbackContract :: !Address, mOutput :: !(Maybe FilePath) } -- !(View_ Address)
  | GetDSAddress  { callbackContract :: !Address, mOutput :: !(Maybe FilePath) } -- !(View_ Address)
  | GetRemaining  { callbackContract :: !Address, mOutput :: !(Maybe FilePath) } -- !(View_ Natural)
  | GetWhitelist  { callbackContract :: !Address, mOutput :: !(Maybe FilePath) } -- !(View_ Whitelist)
  | Document !(Maybe FilePath)
  | Analyze
  | Parse !Address !Address !(Maybe FilePath)
  deriving (Show)

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ printSubCmd
  , printSpecializedSubCmd
  , printSpecializedFA12SubCmd
  , printValidatedExpiringSubCmd
  , initialStorageSubCmd
  , initialStorageValidatedExpiringSubCmd
  , flushForwarderSubCmd
  , validateTransferSubCmd
  , getExpirationSubCmd
  , getDSAddressSubCmd
  , getRemainingSubCmd
  , getWhitelistSubCmd
  , documentSubCmd
  , analyzeSubCmd
  , parseSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> outputOption <*> onelineOption)
      "Dump DS Token Forwarder contract in the form of Michelson code"

    printSpecializedSubCmd =
      mkCommandParser "print-specialized-fa12"
      (PrintSpecialized <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        (L.FutureContract . uncurry L.EpAddress . (, L.def) <$> addressOption "fa12-address" "Address of FA1.2 Token contract") <*>
        outputOption <*>
        onelineOption
      )
      ("Dump FA1.2 Token Forwarder contract, specialized to paricular addresses, " <>
      "in the form of Michelson code")

    printSpecializedFA12SubCmd =
      mkCommandParser "print-specialized"
      (PrintSpecialized <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        (L.FutureContract . uncurry L.EpAddress . (, L.def) <$> addressOption "dstoken-address" "Address of DS Token contract") <*>
        outputOption <*>
        onelineOption
      )
      ("Dump DS Token Forwarder contract, specialized to paricular addresses, " <>
      "in the form of Michelson code")

    printValidatedExpiringSubCmd =
      mkCommandParser "print-validated-expiring"
      (PrintValidatedExpiring <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        (L.FutureContract . uncurry L.EpAddress . (, L.def) <$> addressOption "dstoken-address" "Address of DS Token contract") <*>
        outputOption <*>
        onelineOption
      )
      ("Dump DS Token Forwarder contract, specialized to paricular addresses, " <>
      "with sender validating and expiration, in the form of Michelson code")

    initialStorageSubCmd =
      mkCommandParser "initial-storage"
      (InitialStorage <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        addressOption "dstoken-address" "Address of DS Token contract" <*>
        outputOption
      )
      "Dump initial storage value"

    initialStorageValidatedExpiringSubCmd =
      mkCommandParser "initial-storage-validated-expiring"
      (InitialStorageValidatedExpiring <$>
        addressOption "dstoken-address" "Address of DS Token contract" <*>
        parseStrings "whitelisted-investors" <*>
        parseNatural "token-limit" <*>
        parseTimestamp "expiration-date" <*>
        outputOption
      )
      "Dump initial storage value for validated-expiring forwarder"

    flushForwarderSubCmd =
      mkCommandParser "flush-forwarder"
      (FlushForwarder <$>
        parseNatural "tokens-to-flush" <*>
        outputOption
      )
      "Parameter to flush validated-expiring forwarder"

    validateTransferSubCmd =
      mkCommandParser "validate-transfer"
      (ValidateTransfer <$>
        parseNatural "received-amount" <*>
        (InvestorId . mkMTextUnsafe . fromString <$> parseString "from-user") <*>
        outputOption
      )
      ("Parameter to validate an amount of tokens and its sender for the " <>
      "validated-expiring forwarder. Note: it will fail if invalid and " <>
      "otherwise update the remaining token forwarding limit.")

    getExpirationSubCmd =
      mkCommandParser "get-expiration"
      (GetExpiration <$>
        addressOption "callback-contract" "contract accepting a 'timestamp' callback" <*>
        outputOption
      )
      ("Parameter to view the expiration timestamp, " <>
      "given a contract accepting a 'timestamp")

    getDSAddressSubCmd =
      mkCommandParser "get-ds-address"
      (GetDSAddress <$>
        addressOption "callback-contract" "contract accepting an 'address' callback" <*>
        outputOption
      )
      ("Parameter to view the DS token contract address, " <>
      "given a contract accepting an 'address'")

    getRemainingSubCmd =
      mkCommandParser "get-token-limit"
      (GetRemaining <$>
        addressOption "callback-contract" "contract accepting a 'nat' callback" <*>
        outputOption
      )
      ("Parameter to view the remaining token forwarding limit, " <>
      "given a contract accepting a 'nat'")

    getWhitelistSubCmd =
      mkCommandParser "get-whitelist"
      (GetWhitelist <$>
        addressOption "callback-contract" "contract accepting a '(set string)' callback" <*>
        outputOption
      )
      ("Parameter to view the DS token contract address, " <>
      "given a contract accepting a (set string)")

    documentSubCmd =
      mkCommandParser "document"
      (Document <$> outputOption)
      "Dump contract documentation in Markdown"

    analyzeSubCmd =
      mkCommandParser "analyze"
      (pure Analyze)
      "Analyze DS Token Forwarder contract and print information about it"

    parseSubCmd =
      mkCommandParser "parse"
      (Parse <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        addressOption "dstoken-address" "Address of DS Token contract" <*>
        outputOption
      )
      "Parse and verify a copy of the contract"

    outputOption = Opt.optional $ Opt.strOption $ mconcat
      [ Opt.short 'o'
      , Opt.long "output"
      , Opt.metavar "FILEPATH"
      , Opt.help "Output file"
      ]

    onelineOption = Opt.switch (
      Opt.long "oneline" <>
      Opt.help "Force single line output")


-- Copy-pasted from `morley` CLI parsing.
addressOption :: String -> String -> Opt.Parser Address
addressOption name hInfo =
  Opt.option (Opt.eitherReader parseAddrDo) $ mconcat
  [ Opt.long name
  , Opt.metavar "ADDRESS"
  , Opt.help hInfo
  ]
  where
    parseAddrDo addr =
      either (Left . mappend "Failed to parse address: " . pretty) Right $
      parseAddress $ toText addr

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo = Opt.info (Opt.helper <*> argParser) $ -- versionOption <*>
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "CLI for DS Token Forwarder contract"
  , Opt.header "DS Token Forwarder"
  , Opt.footerDoc usageDoc
  ]
  where
    -- versionOption = Opt.infoOption ("morley-dstoken-contract" <> showVersion version)
    --   (Opt.long "version" <> Opt.help "Show version.")

usageDoc :: Maybe Doc
usageDoc = Just $ mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  morley-dstoken-forwarder-contract print --help", linebreak
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

someLorentzContract :: SomeContract -> L.SomeContract
someLorentzContract (SomeContract (contract' :: FullContract cp st)) =
  case contract' of
    FullContract{..} ->
      assertOpAbsense @cp $
      assertNestedBigMapAbsense @cp $
      assertOpAbsense @st $
      assertNestedBigMapAbsense @st $
      assertContractTypeAbsense @st $
      L.SomeContract $
      L.I @('[(L.Value cp, L.Value st)]) @('[([L.Operation], L.Value st)]) fcCode

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
    -- appendFunc :: (Print text, IsString text, Monoid text) => Maybe FilePath -> text -> IO ()
    -- appendFunc = printFunc appendFileUtf8

    run :: CmdLnArgs -> IO ()
    run = \case
      Print mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline
            DS.forwarderContract
      PrintSpecialized centralWalletAddr' dsTokenContractRef' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            DS.specializedForwarderContract centralWalletAddr' $
            L.toContractRef dsTokenContractRef'
      PrintSpecializedFA12 centralWalletAddr' fa12ContractRef' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Specialized.specializedForwarderContract centralWalletAddr' $
            L.toContractRef fa12ContractRef'
      PrintValidatedExpiring centralWalletAddr' dsTokenContractRef' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            DS.validatedExpiringForwarderContract centralWalletAddr' $
            L.toContractRef dsTokenContractRef'
      InitialStorage centralWalletAddr dsTokenAddr mOutput ->
        writeFunc mOutput $ L.printLorentzValue True $ DS.Storage dsTokenAddr centralWalletAddr
      InitialStorageValidatedExpiring{..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        ValidatedExpiring.mkStorageWithInvestorIds dsTokenAddress whitelist tokenLimit expirationDate
      FlushForwarder {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.WrappedParameter $
        Product.LeftParameter $
        amountToFlush
      ValidateTransfer {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.WrappedParameter $
        Product.RightParameter $
        ValidateReception.Validate $
        ValidateReception.ReceptionParameters receivedAmount fromUser
      GetExpiration {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.GetExpiration $
        toView_ callbackContract
      GetDSAddress {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.WrappedParameter $
        Product.RightParameter $
        ValidateReception.GetDSAddress $
        toView_ callbackContract
      GetRemaining {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.WrappedParameter $
        Product.RightParameter $
        ValidateReception.GetRemaining $
        toView_ callbackContract
      GetWhitelist {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.WrappedParameter $
        Product.RightParameter $
        ValidateReception.GetWhitelist $
        toView_ callbackContract
      Document _ -> error "documentation not implemented"
      Analyze -> do
        let
          printItem :: (L.Text, AnalyzerRes) -> IO ()
          printItem (name, res) =
            putTextLn $ fmt $ nameF (build name) $ build res

        printItem ("DS Token Forwarder Contract", DS.analyzeForwarder)
      Parse centralWalletAddr' contractAddr' mInput -> do
        uContract <- expandContract <$> readAndParseContract mInput
        let tcContracts' =
              Map.singleton
                (case contractAddr' of {ContractAddress addr' -> addr'; _ -> error ("Expected ContractAddress, but got: " <> show contractAddr')})
                (U.Type (toUT (fromSing expectedContractParamT)) noAnn)
        case typeCheckContract tcContracts' uContract of
          Left err -> die $ "Failed to type check contract: " <> show err
          Right typeCheckedContract ->
            case DS.verifyForwarderContract centralWalletAddr' (L.toContractRef contractAddr') $ someLorentzContract typeCheckedContract of
              -- ContractRef
              Left err -> die err
              Right () -> putStrLn ("Contract verified successfully!" :: Text)

    expectedContractParamT :: Sing (L.ToT DSToken.Parameter)
    expectedContractParamT = sing -- sing @(L.ToT DSToken.Parameter))

    asParameterType :: ValidatedExpiring.Parameter -> ValidatedExpiring.Parameter
    asParameterType = id


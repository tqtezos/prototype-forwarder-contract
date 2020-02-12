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
import Data.Typeable (eqT, Typeable, (:~:)(..))

import Tezos.Address (Address(..))
import qualified Tezos.Address as Tezos
import qualified Tezos.Core as Core (parseTimestamp)
import Lorentz (Timestamp, ToT, IsoValue)
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
import qualified Lorentz.Contracts.Forwarder.DS.V1.Specialized as DSSpecialized
import qualified Lorentz.Contracts.Forwarder.DS.V1.ValidatedExpiring as DS
import qualified Lorentz.Contracts.Forwarder.DS.V1.ValidatedExpiring as ValidatedExpiring
import qualified Lorentz.Contracts.Forwarder.DS.V1.Validated as Validated
import qualified Lorentz.Contracts.Forwarder.DS.V1 as DS
import Lorentz.Contracts.Forwarder.TestScenario
import qualified Lorentz.Contracts.DS.V1 as DSToken
import Lorentz.Contracts.DS.V1.Registry.Types (InvestorId(..))
import Morley.Nettest (NettestClientConfig(..), NettestScenario, NettestT)
import qualified Morley.Nettest as NT

import NettestHelpers
import Lorentz.Contracts.View
import qualified Lorentz.Contracts.Product as Product
import qualified Lorentz.Contracts.Expiring as Expiring
import qualified Lorentz.Contracts.Validate.Reception as ValidateReception

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
  = Print !(Maybe FilePath) !Bool
  | PrintSpecialized !Address !(L.FutureContract DSToken.Parameter) !(Maybe FilePath) !Bool
  | PrintSpecializedFA12 !Address !Address !(Maybe FilePath) !Bool
  | PrintValidatedExpiring !Address !(L.FutureContract DSToken.Parameter) !(Maybe FilePath) !Bool
  | PrintValidated !Address !(L.FutureContract DSToken.Parameter) !(Maybe FilePath) !Bool
  | InitialStorage !Address !Address !(Maybe FilePath)
  | InitialStorageValidatedExpiring
      { whitelist :: ![String]
      , expirationDate :: !Timestamp
      , mOutput :: !(Maybe FilePath)
      }
  | InitialStorageValidated
      { whitelist :: ![String]
      , mOutput :: !(Maybe FilePath)
      }
  | FlushForwarder { amountToFlush :: !Natural, mOutput :: !(Maybe FilePath) }
  | ValidateTransfer { fromUser :: !InvestorId, mOutput :: !(Maybe FilePath) }
  | GetExpiration { callbackContract :: !Address, mOutput :: !(Maybe FilePath) } -- !(View_ Address)
  | GetWhitelist  { callbackContract :: !Address, mOutput :: !(Maybe FilePath) } -- !(View_ Whitelist)
  | Document !(Maybe FilePath)
  | Analyze
  | Parse !Address !Address !(Maybe FilePath)
  | DeployAndTest
  deriving (Show)

toFutureDSToken :: Address -> L.FutureContract DSToken.Parameter
toFutureDSToken =
  L.FutureContract . L.callingDefTAddress . L.toTAddress @DSToken.Parameter

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ printSubCmd
  , printSpecializedSubCmd
  , printSpecializedFA12SubCmd
  , printValidatedExpiringSubCmd
  , printValidatedSubCmd
  , initialStorageSubCmd
  , initialStorageValidatedExpiringSubCmd
  , initialStorageValidatedSubCmd
  , flushForwarderSubCmd
  , validateTransferSubCmd
  , getExpirationSubCmd
  , getWhitelistSubCmd
  , documentSubCmd
  , analyzeSubCmd
  , parseSubCmd
  , deployAndTestSubCmd
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

    printSpecializedSubCmd =
      mkCommandParser "print-specialized"
      (PrintSpecialized <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        (toFutureDSToken <$> parseAddress "dstoken-address" "Address of DS Token contract") <*>
        outputOption <*>
        onelineOption
      )
      ("Dump DS Token Forwarder contract, specialized to paricular addresses, " <>
      "in the form of Michelson code")

    printValidatedExpiringSubCmd =
      mkCommandParser "print-validated-expiring"
      (PrintValidatedExpiring <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        (toFutureDSToken <$> parseAddress "dstoken-address" "Address of DS Token contract") <*>
        outputOption <*>
        onelineOption
      )
      ("Dump DS Token Forwarder contract, specialized to paricular addresses, " <>
      "with sender validation and expiration, in the form of Michelson code")

    printValidatedSubCmd =
      mkCommandParser "print-validated"
      (PrintValidated <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        (toFutureDSToken <$> parseAddress "dstoken-address" "Address of DS Token contract") <*>
        outputOption <*>
        onelineOption
      )
      ("Dump DS Token Forwarder contract, specialized to paricular addresses, " <>
      "with sender validation, in the form of Michelson code")

    initialStorageSubCmd =
      mkCommandParser "initial-storage"
      (InitialStorage <$>
        parseAddress "central-wallet" "Address of central wallet" <*>
        parseAddress "dstoken-address" "Address of DS Token contract" <*>
        outputOption
      )
      "Dump initial storage value"

    initialStorageValidatedExpiringSubCmd =
      mkCommandParser "initial-storage-validated-expiring"
      (InitialStorageValidatedExpiring <$>
        parseStrings "whitelisted-investors" <*>
        parseTimestamp "expiration-date" <*>
        outputOption
      )
      "Dump initial storage value for validated-expiring forwarder"

    initialStorageValidatedSubCmd =
      mkCommandParser "initial-storage-validated"
      (InitialStorageValidated <$>
        parseStrings "whitelisted-investors" <*>
        outputOption
      )
      "Dump initial storage value for validated forwarder"

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
        (InvestorId . mkMTextUnsafe . fromString <$> parseString "from-user") <*>
        outputOption
      )
      ("Parameter to validate an amount of tokens and its sender for the " <>
      "validated forwarder. Note: it will fail if invalid.")

    getExpirationSubCmd =
      mkCommandParser "get-expiration"
      (GetExpiration <$>
        parseAddress "callback-contract" "contract accepting a 'timestamp' callback" <*>
        outputOption
      )
      ("Parameter to view the expiration timestamp, " <>
      "given a contract accepting a 'timestamp")

    getWhitelistSubCmd =
      mkCommandParser "get-whitelist"
      (GetWhitelist <$>
        parseAddress "callback-contract" "contract accepting a '(set string)' callback" <*>
        outputOption
      )
      ("Parameter to view the whitelist for the validated forwarder, " <>
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
        parseAddress "central-wallet" "Address of central wallet" <*>
        parseAddress "dstoken-address" "Address of DS Token contract" <*>
        outputOption
      )
      "Parse and verify a copy of the contract"

    deployAndTestSubCmd =
      mkCommandParser "deploy-test"
      (pure DeployAndTest)
      "Deploy and test different ledgers and forwarder contracts"

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
  , Opt.progDesc "CLI for DS Token Forwarder contract"
  , Opt.header "DS Token Forwarder"
  , Opt.footerDoc usageDoc
  ]

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
          (L.I fcCode :: L.Contract param (L.Value st))

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
      PrintSpecializedFA12 centralWalletAddr' fa12ContractAddr' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Specialized.specializedForwarderContract centralWalletAddr' $
            fa12ContractAddr'
      PrintValidatedExpiring centralWalletAddr' dsTokenContractRef' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            DS.validatedExpiringForwarderContract centralWalletAddr' $
            L.toContractRef dsTokenContractRef'
      PrintValidated centralWalletAddr' dsTokenContractRef' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            Validated.validatedForwarderContract centralWalletAddr' $
            L.toContractRef dsTokenContractRef'
      InitialStorage centralWalletAddr dsTokenAddr mOutput ->
        writeFunc mOutput $ L.printLorentzValue True $ DS.Storage dsTokenAddr centralWalletAddr
      InitialStorageValidatedExpiring{..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        ValidatedExpiring.mkStorageWithInvestorIds whitelist expirationDate
      InitialStorageValidated{..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        Validated.mkStorageWithInvestorIds whitelist
      FlushForwarder {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asValidatedParameterType $
        Product.LeftParameter $
        amountToFlush
      ValidateTransfer {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asValidatedParameterType $
        Product.RightParameter $
        ValidateReception.Validate $
        fromUser
      GetExpiration {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asParameterType $
        Expiring.GetExpiration $
        toView_ (L.toTAddress callbackContract)
      GetWhitelist {..} ->
        writeFunc mOutput $
        L.printLorentzValue True $
        asValidatedParameterType $
        Product.RightParameter $
        ValidateReception.GetWhitelist $
        toView_ (L.toTAddress callbackContract)
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
        let contractRef = L.callingDefTAddress $ L.toTAddress @DSToken.Parameter contractAddr'
        let verifyForwarder = DS.verifyForwarderContract centralWalletAddr' contractRef
        case typeCheckContract tcContracts' uContract of
          Left err -> die $ "Failed to type check contract: " <> show err
          Right typeCheckedContract ->
            case verifyForwarder $ someLorentzContract @DSSpecialized.Parameter typeCheckedContract of
              -- ContractRef
              Left err -> die err
              Right () -> putStrLn ("Contract verified successfully!" :: Text)
      DeployAndTest -> deployAndTest

    deployAndTest :: IO ()
    deployAndTest = do
      dsId <- genContractId "er"
      fwdId <- genContractId "fwd"
      let
        scenario :: NettestScenario
        scenario = NT.uncapsNettest $ do
          dsAddress <- originateDS dsId
          cw <- NT.newAddress "centralWallet"
          fwdAddress <- originateForwarder fwdId dsAddress cw
          testScenario $
            TestScenarioParameters
              { tspDSToken = dsAddress
              , tspCentralWallet = cw
              , tspForwarder = fwdAddress
              }

      -- Quick check on that scenario is sane
      NT.runNettestViaIntegrational scenario
      putTextLn "Test precheck passed, running on test network"
      NT.runNettestClient nettestConfig scenario
      putTextLn "Test suite completed"


    testScenario :: Monad m => TestScenarioParameters -> NettestT m ()
    testScenario params = do
      runTestScenario params
      NT.comment "Test passed successfully"

    expectedContractParamT :: Sing (L.ToT DSToken.Parameter)
    expectedContractParamT = sing -- sing @(L.ToT DSToken.Parameter))

    asParameterType :: ValidatedExpiring.Parameter -> ValidatedExpiring.Parameter
    asParameterType = id

    asValidatedParameterType :: Validated.Parameter -> Validated.Parameter
    asValidatedParameterType = id

-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import System.IO hiding (putStrLn, print)
import Data.String
import Prelude hiding (putStrLn, show, print)

-- import Data.Version (showVersion)
import Fmt (pretty, fmt, build, nameF)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import qualified Data.Text.Lazy as LT
import qualified Data.Map as Map
import Data.Singletons

-- import Michelson.Text (MText, mkMText, mt)
-- import Paths_morley_dstoken (version)
import Tezos.Address (Address(..), parseAddress, formatAddress)
import Tezos.Crypto
import qualified Lorentz as L
import Michelson.Analyzer (AnalyzerRes)
import Util.IO (appendFileUtf8, hSetTranslit, writeFileUtf8)
import Universum.Print
import Universum.String
import Universum.Lifted
import Universum.Exception

import Michelson.Macro
import Michelson.Runtime
import Michelson.TypeCheck
import qualified Michelson.Untyped.Type as U
import Michelson.Untyped.Annotation (noAnn)
import Michelson.Typed.T
import qualified Lorentz.Contracts.Forwarder.DS.V1.Specialized as DS
import qualified Lorentz.Contracts.Forwarder.DS.V1 as DS
import qualified Lorentz.Contracts.DS.V1 as DSToken

deriving instance Show (L.FutureContract p)

data CmdLnArgs
  = Print !(Maybe FilePath) !Bool
  | PrintSpecialized !Address !(L.FutureContract DSToken.Parameter) !(Maybe FilePath) !Bool
  | InitialStorage !Address !Address !(Maybe FilePath)
  | Document !(Maybe FilePath)
  | Analyze
  | Parse !Address !Address !(Maybe FilePath)
  deriving (Show)

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.subparser $ mconcat
  [ printSubCmd
  , printSpecializedSubCmd
  , initialStorageSubCmd
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
      mkCommandParser "print-specialized"
      (PrintSpecialized <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        (L.FutureContract <$> addressOption "dstoken-address" "Address of DS Token contract") <*>
        outputOption <*>
        onelineOption
      )
      "Dump DS Token Forwarder contract, specialized to paricular addresses, in the form of Michelson code"

    initialStorageSubCmd =
      mkCommandParser "initial-storage"
      (InitialStorage <$>
        addressOption "central-wallet" "Address of central wallet" <*>
        addressOption "dstoken-address" "Address of DS Token contract" <*>
        outputOption
      )
      "Dump initial storage value"

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
    appendFunc :: (Print text, IsString text, Monoid text) => Maybe FilePath -> text -> IO ()
    appendFunc = printFunc appendFileUtf8

    run :: CmdLnArgs -> IO ()
    run = \case
      Print mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline
            -- DS.forwarderCompilationWay
            DS.forwarderContract
      PrintSpecialized centralWalletAddr' dsTokenContractRef' mOutput forceOneline ->
        writeFunc mOutput $
          L.printLorentzContract
            forceOneline $
            -- DS.specializedForwarderCompilationWay $
            DS.specializedForwarderContract centralWalletAddr' $
            L.toContractRef dsTokenContractRef'
      InitialStorage centralWalletAddr dsTokenAddr mOutput -> do
        writeFunc mOutput $ L.printLorentzValue True $ DS.Storage dsTokenAddr centralWalletAddr
      -- Document mOutput -> do
      --   writeFunc mOutput $ LT.strip DS.forwarderDocumentation
      Analyze -> do
        let
          printItem :: (L.Text, AnalyzerRes) -> IO ()
          printItem (name, res) =
            putTextLn $ fmt $ nameF (build name) $ build res

        printItem ("DS Token Forwarder Contract", DS.analyzeForwarder)
      xs@(Parse centralWalletAddr' contractAddr' mInput) -> do
        uContract <- expandContract <$> readAndParseContract mInput
        let tcContracts' =
              Map.singleton
                (case contractAddr' of {ContractAddress addr' -> addr'; _ -> error ("Expected ContractAddress, but got: " <> show contractAddr')})
                (U.Type (toUT (fromSing expectedContractParamT)) noAnn)
        case typeCheckContract tcContracts' uContract of
          Left err -> die $ "Failed to type check contract: " <> show err
          Right typeCheckedContract ->
            case DS.verifyForwarderContract centralWalletAddr' (L.toContractRef contractAddr') typeCheckedContract of
              -- ContractRef
              Left err -> die err
              Right () -> putStrLn ("Contract verified successfully!" :: Text)

    expectedContractParamT :: Sing (L.ToT DSToken.Parameter)
    expectedContractParamT = sing -- sing @(L.ToT DSToken.Parameter))

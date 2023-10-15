{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : WATMint
-- Description : Whitelisted Ascending Token Minting
-- (1) @Whitelisted@ - selective and exclusive process.
-- (2) @Ascending@ - pricing model that increases after every specific no. of mints.
-- (3) @Token Minting@ - creation of new tokens/NFTs.
-- Copyright   : (c) Marius Georgescu, 2023
-- License     : GPL-3
-- Maintainer  : georgescumarius@live.com
module Contracts.WATMint where

import Contracts.Samples.NFT qualified as NFT
import Control.Monad.Error.Class (liftEither)
import Data.Either.Extra (maybeToEither)
import Data.List (drop)
import Data.Map qualified as Map
import GHC.Real (Integral (toInteger))
import Jambhala.CLI.Types (ValidatorContract (unValidatorContract))
import Jambhala.Plutus
import Jambhala.Utils
import Ledger (toTxInfoTxOut, toValidatorHash)
import Ledger.Tx.Constraints (mustPayToOtherScriptWithInlineDatum)
import Plutus.Script.Utils.Value
  ( AssetClass (AssetClass),
    assetClass,
    assetClassValue,
    assetClassValueOf,
  )
import Plutus.V1.Ledger.Address as Address
  ( pubKeyHashAddress,
  )
import Plutus.V2.Ledger.Api (OutputDatum (OutputDatum))
import PlutusTx.Builtins (divideInteger)

--  * Description

-- | The minting price of the NFT collection starts with value of 'BasePrice' , and increases with 'Step' after 'Threshold' tickets are minted, until 'MaxSupply' max no of tickets are minted.
--  For the collection minting to start:
--  (1) The 'Issuer' should  first choose the parameters of the model based on the collection's size and pricing targets. (View on  https://www.desmos.com/calculator  P(n)=B+S\ *\operatorname{floor}((n-1)/\ T)/)
--  (2) The 'Issuer' should parameterize the contract with the 'AssetClass' of a NFT he owns.
--  (3) The 'Issuer' should lock a UTXO with the specified NFT and a datum containing the 'Whitelist' (hashes of vouchers) . The length of the 'Whitelist' must be equal witht he 'MaxSupply' parameter.
--  (4) The 'Issuer' distributes the vouchers in a method of is choice.
--  In order to mint an NFT the user must have previously received a voucher code 'VoucherCode'.

--  * Declaring Types
type Issuer = PubKeyHash

type ComeAndGoProofToken = AssetClass

type BasePrice = Integer

type Step = Integer

type Threshold = Integer

type Count = Integer

type MaxSupply = Integer

-- | Script Parameters Data Type
data ScriptParams = ScriptParams
  { issuer :: Issuer,
    cngpToken :: ComeAndGoProofToken,
    basePrice :: BasePrice,
    step :: Step,
    threshold :: Threshold,
    maxSupply :: Integer
  }
  deriving (Generic, FromJSON, ToJSON)

makeLift ''ScriptParams --  Generate Lift instance(s) for the above via Template Haskell.

-- | Datum Data Type
data WATMDatum = WATMDatum
  { fwMintPolicySymbol :: CurrencySymbol,
    whitelist :: Whitelist,
    watmScriptHash :: ValidatorHash
  }
  deriving (Generic, FromJSON, ToJSON, Show)

type Whitelist = [BuiltinByteString]

makeIsDataIndexed ''WATMDatum [('WATMDatum, 0)]

-- | Redeemer Data Type
type VoucherCode = BuiltinByteString

type VoucherHash = BuiltinByteString

-- * Lambda Validator

-- | To pass validation, a submitted transaction must satisfy the following conditions:
-- Tx must contain a valid state token input (CnGP Token)
-- Tx must contain a redeemer with a voucher code present in the whitelist of the datum of the current output.
-- Tx  must mint an NFT with voucher code as token name.
-- Tx must lock (in one or more outputs) at least the price value (determined using the pricemodel) at the issuer address (specified in ScriptParams).
-- Tx must have an output locked at the script address (specified in datum) containing:
--  (1) state token (CnGP Token)
--  (2) updated datum  (vocher code consumed from whitelist)
{-# INLINEABLE watmLambda #-}
watmLambda :: ScriptParams -> WATMDatum -> VoucherCode -> ScriptContext -> Bool
watmLambda ScriptParams {..} watmDatum@WATMDatum {fwMintPolicySymbol, whitelist} voucherCode (ScriptContext TxInfo {..} _) =
  let count = maxSupply #- plength whitelist -- The issuer must make sure the max supply is equal with the vouchers in whitelist.
      nextPrice = priceModel basePrice step threshold count
      voucherHash = sha2_256 voucherCode
   in pand
        [ "Available vouchers must exist !" `traceIfFalse` (count #>= 0),
          "Redeemer voucher code must be in whitelist" `traceIfFalse` (voucherHash `pelem` whitelist),
          "Transaction must mint an NFT with voucher code as token name" `traceIfFalse` checkMinting fwMintPolicySymbol voucherCode txInfoMint,
          "Transaction must contain a valid state token input" `traceIfFalse` checkStateInput cngpToken txInfoInputs,
          "Trasaction must contain a valid state token output with updated whitelist" `traceIfFalse` checkStateOuput cngpToken watmDatum voucherHash txInfoOutputs,
          "Correct price must be paid to the issuer " `traceIfFalse` checkPaymentInOutputs issuer nextPrice txInfoOutputs
        ]

-- | Helper function to calculate a nextPrice .
{-# INLINEABLE priceModel #-}
priceModel :: BasePrice -> Step -> Threshold -> Count -> Integer
priceModel base step threshold count = base #+ step #* (count `divideInteger` threshold)

-- | This function checks that only 1 token, of a given 'CurrencySymbol' with a specific 'TokenName' is minted.
{-# INLINEABLE checkMinting #-}
checkMinting :: CurrencySymbol -> VoucherCode -> Value -> Bool
checkMinting policy voucher minted =
  let nftName = TokenName voucher
   in (policy, nftName, 1) `isInValue` minted

-- | Helper function to check that the correct quantity of the given token is minted.
{-# INLINEABLE isInValue #-}
isInValue :: (CurrencySymbol, TokenName, Integer) -> Value -> Bool
isInValue (cs, tn, q) = pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q #>= q') . flattenValue


-- | Helper function to check if a 'TxOut' contains exactly 1 'ComeAndGoProofToken'.
{-# INLINEABLE isNFTinTxOut #-}
isNFTinTxOut :: ComeAndGoProofToken -> TxOut -> Bool
isNFTinTxOut (AssetClass (cs, tn)) (TxOut _ value _ _) = isInValue (cs, tn, 1) value

-- | This function checks if the 'ComeAndGoProofToken' is in the list of inputs.
{-# INLINEABLE checkStateInput #-}
checkStateInput :: ComeAndGoProofToken -> [TxInInfo] -> Bool
checkStateInput cng = pany (isNFTinTxOut cng . txInInfoResolved)

-- | This function returns an updated 'WATMDatum' (eliminating the consumed 'VoucherCode' from the 'Whitelist').
{-# INLINEABLE consumeVoucher #-}
consumeVoucher :: VoucherHash -> WATMDatum -> WATMDatum
consumeVoucher voucherHash WATMDatum {..} = WATMDatum fwMintPolicySymbol (filter (#/= voucherHash) whitelist) watmScriptHash

-- | This function checks if a TxOut
--  (1) has the updated 'WATMDatum'.(with 'VoucherCode' consumed)
--  (2) is locked to a script address specified in the 'WATMDatum'.
--  (3) contains the 'ComeAndGoProofToken'.
{-# INLINEABLE isTxOutToScriptWithNFTandUpdatedDatum #-}
isTxOutToScriptWithNFTandUpdatedDatum :: ComeAndGoProofToken -> WATMDatum -> VoucherHash -> TxOut -> Bool
isTxOutToScriptWithNFTandUpdatedDatum (AssetClass (cs, TokenName tn)) currentDatum voucherHash (TxOut address value outputDatum _) =
  let updatedDatum = consumeVoucher voucherHash currentDatum
      hasCorrectDatum = case outputDatum of
        OutputDatum da -> toBuiltinData updatedDatum #== getDatum da
        _ -> False -- Datum must exsist and must be inlined
      hasCorrectAddress = toValidatorHash address #== Just (watmScriptHash currentDatum)
      hasCorrectValue = isInValue (cs, TokenName tn, 1) value
   in pand
        [hasCorrectDatum, hasCorrectAddress, hasCorrectValue]

-- | This function checks if any 'TxOut' matches @isTxOutToScriptWithNFTandUpdatedDatum@ conditions.
{-# INLINEABLE checkStateOuput #-}
checkStateOuput :: ComeAndGoProofToken -> WATMDatum -> VoucherHash -> [TxOut] -> Bool
checkStateOuput nft datum vh = pany (isTxOutToScriptWithNFTandUpdatedDatum nft datum vh)

-- | This function checks if in a list of outputs is at least a specific amount of lovelaces locked to a specific 'PublicKeyHash'.
-- It takes one argument @pkh@ of type'PubKeyHash', one argument @n@ of type 'Integer', and a '[TxOut]'.
-- It returns a 'True' if the sum of lovelaces locked at @pkh@'s address is at least @n@
{-# INLINEABLE checkPaymentInOutputs #-}
checkPaymentInOutputs :: PubKeyHash -> Integer -> [TxOut] -> Bool
checkPaymentInOutputs pkh lovelaces txOuts =
  let outsToAddres = filter ((#== pubKeyHashAddress pkh) . txOutAddress) txOuts
      totalVal = psum (txOutValue #<$> outsToAddres)
   in assetClassValueOf totalVal (assetClass adaSymbol adaToken) #>= lovelaces

-- | Untyped version of the validator lambda.
{-# INLINEABLE untypedLambda #-}
untypedLambda :: ScriptParams -> UntypedValidator
untypedLambda = mkUntypedValidator . watmLambda

-- | The type synonym for the compiled spending validator script.
type WATMValidator = ValidatorContract "watm-validator"

-- | Function for producing the compiled spending validator script.
compileValidator :: ScriptParams -> WATMValidator
compileValidator params = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode params)

-- | Synonym for the forwarding minting policy script.
type WATMPolicy = MintingContract "watm-fmp"

-- | Function for producing the compiled forward minting policy script.
compilePolicy :: ScriptParams -> WATMPolicy
compilePolicy = getFwdMintingPolicy . compileValidator

-- To produce the finished minting policy, select an arbitrary UTxO at your address to consume during the mint.
-- Apply unsafeMkTxOutRef to its TxHash and TxIx values to construct a TxOutRef value, and provide this as argument to the parameterized policy.
-- This UTxO must be included as an input to your minting transaction.
-- for emulator use: "899b40a640d4d3df5bb4a85b0d03be7df0509bcd7f6c1e99075423852a35a2a4" 10
nftCurrencySymbol :: CurrencySymbol
nftCurrencySymbol = getCurrencySymbol $ NFT.compileScript (NFT.PolicyParam (unsafeMkTxOutRef "c7dfd04e8e757bd893ea8a04f90dfa01a5ff6c1e04b2dc314394dabc1189fac2" 1) "jambtoken")

nftAssetClass :: AssetClass
nftAssetClass = AssetClass (nftCurrencySymbol, TokenName "jambtoken")

-- | Example of 'ScriptParams'
sampleScriptParams :: ScriptParams
sampleScriptParams =
  ScriptParams
    { issuer = "6ed20897111f7357220866dc58a0e6a74be213d7d32a8e991af07627",
      cngpToken = nftAssetClass,
      basePrice = 10,
      step = 2,
      threshold = 2,
      maxSupply = 10
    }

-- | Example of 'WATMPolicy'
sampleCompiledPolicy :: WATMPolicy
sampleCompiledPolicy = compilePolicy sampleScriptParams

-- | Example of 'WATMValidator'
sampleCompiledValidator :: WATMValidator
sampleCompiledValidator = compileValidator sampleScriptParams

-- | Export Contract to Jambhala

-- | Define minting policy exports value for use with `jamb` CLI.
watmPolicyExports :: JambExports
watmPolicyExports = export (defExports sampleCompiledPolicy)

-- | These are some examples of voucher codes, but they can be any string
voucherCodes :: [String]
voucherCodes = ["ADA", "BTC", "ETH", "LTC", "SOL", "ERGO", "ALGO", "XRP", "BNB", "XLM"]

-- | Define spending validator exports value for use with `jamb` CLI.
watmValidatorExports :: JambExports
watmValidatorExports =
  export
    ( defExports
        sampleCompiledValidator
    )
      { dataExports =
          [ mkWATMDatum sampleCompiledValidator voucherCodes `toJSONfile` "watmDatum",
            mkWATMDatum sampleCompiledValidator ["BTC", "ETH", "LTC", "SOL", "ERGO", "ALGO", "XRP", "BNB", "XLM"] `toJSONfile` "watmDatum1",
            mkWATMDatum sampleCompiledValidator (drop 2 voucherCodes) `toJSONfile` "watmDatum2",
            mkWATMDatum sampleCompiledValidator (drop 3 voucherCodes) `toJSONfile` "watmDatum3",
            mkWATMDatum sampleCompiledValidator (drop 4 voucherCodes) `toJSONfile` "watmDatum3",
            mkWATMDatum sampleCompiledValidator (drop 5 voucherCodes) `toJSONfile` "watmDatum3",
            mkWATMDatum sampleCompiledValidator (drop 6 voucherCodes) `toJSONfile` "watmDatum3",
            mkWATMDatum sampleCompiledValidator (drop 7 voucherCodes) `toJSONfile` "watmDatum3",
            mkWATMRedeemer "ADA" `toJSONfile` "ADARedeemer",
            mkWATMRedeemer "BTC" `toJSONfile` "BTCRedeemer",
            mkWATMRedeemer "ETH" `toJSONfile` "ETHRedeemer",
            mkWATMRedeemer "LTC" `toJSONfile` "LTCRedeemer",
            mkWATMRedeemer "SOL" `toJSONfile` "SOLRedeemer",
            mkWATMRedeemer "ERGO" `toJSONfile` "ERGOedeemer",
            mkWATMRedeemer "ALGO" `toJSONfile` "ALGOedeemer",
            () `toJSONfile` "unit"
          ],
        emulatorTest = test
      }

-- | Helper function for assembling a 'WATMDatum'.
mkWATMDatum :: WATMValidator -> [String] -> WATMDatum
mkWATMDatum validator codes =
  WATMDatum
    { fwMintPolicySymbol = getFwdMintingPolicyId validator,
      whitelist = sha2_256 . stringToBuiltinByteString <$> codes,
      watmScriptHash = validatorHash $ unValidatorContract validator
    }

-- | Helper function for assembling a redeemer 'VoucherCode'.
mkWATMRedeemer :: String -> VoucherCode
mkWATMRedeemer = stringToBuiltinByteString

-- 5. Define Emulator Component

-- | Define `ValidatorEndpoints` instance for the validator synonym (`MintingEndpoint` instance not required for the policy synonym).
instance ValidatorEndpoints WATMValidator where
  data GiveParam WATMValidator = Register
    { params :: ScriptParams,
      datum :: WATMDatum
    }
    deriving (Generic, FromJSON, ToJSON)
  data GrabParam WATMValidator = Claim
    { redeemerCode :: BuiltinByteString,
      scriptParams :: ScriptParams
    }
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam WATMValidator -> ContractM WATMValidator ()
  give Register {..} = do
    let validator = compileValidator params
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor validator, -- lookups for the associated forwarding minting policy are automatically included
          constraints = mustPayScriptWithDatum validator datum (lovelaceValueOf 2_000_000 <> assetClassValue nftAssetClass 1)
        }
    logStr . logHL $
      printf
        "Registered vouchers |%s| to  %s "
        (show . whitelist $ datum)
        (show . watmScriptHash $ datum)

  grab :: GrabParam WATMValidator -> ContractM WATMValidator ()
  grab Claim {scriptParams, redeemerCode} = do
    let watmvalidator = compileValidator scriptParams
    let watmpolicy = compilePolicy scriptParams
    scriptUtxos <- getUtxosAt watmvalidator
    let hasNFT = (== 1) . flip assetClassValueOf nftAssetClass . txOutValue . toTxInfoTxOut
    let nftUtxo = Map.filter hasNFT scriptUtxos
    case Map.toList nftUtxo of
      [(oref, dTxOut)] -> do
        (_dhash, datumFromQuery) <- liftEither . maybeToEither "No DatumFromQuery" $ getDecoratedTxOutDatum dTxOut
        outputDatum <- liftEither . maybeToEither "No Datum" $ getDatumInDatumFromQuery datumFromQuery
        watmDatum <- liftEither . maybeToEither "Convertion from BuiltinData failed" $ fromBuiltinData . getDatum $ outputDatum
        let count = maxSupply scriptParams - (toInteger . length . whitelist $ watmDatum)
        let updatedDatum = consumeVoucher (sha2_256 redeemerCode) watmDatum
        let mintPrice = priceModel (basePrice scriptParams) (step scriptParams) (threshold scriptParams) count
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor watmvalidator `andUtxos` nftUtxo,
              constraints =
                mconcat
                  [ oref `mustBeSpentWith` redeemerCode, --  CnGPToken output must be spent
                    mustPayToOtherScriptWithInlineDatum (validatorHash . unValidatorContract $ watmvalidator) (mkDatum updatedDatum) (lovelaceValueOf 5_000_000 <> assetClassValue nftAssetClass 1), -- CnGPToken must be locked back to the script with updated datum
                    mustPayPKH (issuer scriptParams) (lovelaceValueOf (mintPrice * 1000000)), -- NFT price acccording to the price model and current state must be payed to the issuer.
                    mustMint watmpolicy (TokenName redeemerCode) 1 -- NFT must be minted
                  ]
            }
        logStr . logHL $ printf "Minted NFT with redeemer code %s at price %s ADA" (show redeemerCode) (show mintPrice)
      _ -> logStr . logHL $ "No or invalid CnGPT"

test :: EmulatorTest
test =
  initEmulator @NFT.NFTMinting 1 [NFT.Mint "jambtoken" `forWallet` 1] -- mint NFT to wallet 1
    <> initEmulator @WATMValidator -- Register whitelist to the script
      2
      [ Register sampleScriptParams (mkWATMDatum sampleCompiledValidator voucherCodes) `fromWallet` 1,
        Claim "ADA" sampleScriptParams `toWallet` 2,
        Claim "BTC" sampleScriptParams `toWallet` 1,
        Claim "SOL" sampleScriptParams `toWallet` 2,
        Claim "ETH" sampleScriptParams `toWallet` 1,
        Claim "XRP" sampleScriptParams `toWallet` 2
      ]

logHL :: String -> String
logHL = ("###########################################################" ++)
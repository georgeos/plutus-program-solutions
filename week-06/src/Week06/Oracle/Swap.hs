{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import           Week06.Oracle.Core
import           Week06.Oracle.Funds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- Parameters: Oracle, Address
-- Datum is PubKeyHash (pkh of the Seller)
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Checks if there is exactly one oracle determined by addr parameter
    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    -- Gets the datum of the Oracle Input (1.75)
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    -- Checks all script inputs equal to two
    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        -- Gets all script inputs
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    -- Gets the available amount of lovelace to be swapped. 
    minPrice :: Integer
    minPrice =
      let
        -- findOwnInput Gets input is being validated. In this case Swap input
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        -- To compute price: 100 * 1.75 = 175
        price lovelaceIn oracleValue'

    -- Check if buyer paid to seller enough using oAsset (which is value of exchange 1.75)
    sellerPaid :: Bool
    sellerPaid =
      let
        -- Gets all value paid by Seller
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash -- PubKeyHash of the Seller
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

-- Contract created for the Seller. If Seller wants to provide a swap
-- Take two parameter: Oracle and Integer (amount of lovelace he wants to offer)
offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    -- Get public key of the Seller
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- Lock amount of lovelace in the swap contract
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

-- Find swaps that satisfy a predicate
findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    -- Give a list of all UTXOs that sits at swapAddress
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    -- Get pubkeyhash from datum attached to the Output
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        -- Try to deserialize from Datum to PubKeyHash
        PlutusTx.fromBuiltinData d

    -- Return all UTXO including the PubKeyHash (if pubkeyhash is equal to parameter)
    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        -- guard: evaluates if is True, then continue else not continue.
        guard $ p pkh
        return (oref, o, pkh)

-- Allow a Seller to retrieve his funds
retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    -- Get seller publickeyhash
    pkh <- pubKeyHash <$> ownPubKey
    xs  <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                -- Combine all constraints using mconcat
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

-- Exchange (swap)
useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    -- Get all funds from Buyer wallet
    funds <- ownFunds
    -- Check how many funds have for that token (oAsset)
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            -- Get pubkeyhash of the buyer
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            -- Look for all swaps not owned by buyer
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of  -- x represents oracle value: 1.75
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                        -- Total value
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        -- Price to pay
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  -- Consume swap input
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  -- Pay to the oracle
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    v                                                                             <>
                                  -- Pay to the seller of lovelace
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    -- Validate buyer funds (mnt 175) is greather than price buyer have to pay
    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer"
        offerSwap oracle amt

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError

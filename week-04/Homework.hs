{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace payment1 payment2 = do

    handle <- activateContractWallet (Wallet 1) payContract

    let convert :: Wallet -> PubKeyHash
        convert = pubKeyHash . walletPubKey
        pubKeyHash2 = convert (Wallet 2)
        params1 = PayParams{
            ppRecipient = pubKeyHash2
        ,   ppLovelace = payment1
        }
        params2 = PayParams{
            ppRecipient = pubKeyHash2
        ,   ppLovelace = payment2
        }
 
    callEndpoint @"pay" handle params1
    void $ Emulator.waitNSlots 1

    callEndpoint @"pay" handle params2
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000

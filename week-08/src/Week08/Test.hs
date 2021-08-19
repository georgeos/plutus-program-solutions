{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week08.Test where

import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Plutus.Contract.Test
import           Ledger                             hiding (singleton)
import           Ledger.Value

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

-- List of wallets
wallets :: [Wallet]
wallets = [w1, w2]

-- For wallet 1: "aa" "A", for wallet 2: "bb" "B"
tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

f :: Integer -> Integer
f = (+ (- 4))

a :: [(Wallet, Wallet)]
a = [(v,w) | v <- wallets, w <- wallets]
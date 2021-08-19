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

module Spec.Model
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Week08.TokenSale                   (TokenSale (..), TSStartSchema, TSUseSchema, startEndpoint, useEndpoints)

-- Current Token Sale State (of the contract)
data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show

makeLenses ''TSState

-- Each wallet run Token Sale contract
newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel

tests :: TestTree
tests = testProperty "token sale model" prop_TS

-- This provides how should behave and link to the contract
instance ContractModel TSModel where

    -- Datatype which corresponds to actions that quickCheck will generate
    data Action TSModel =
            -- Wallet as an additional parameter in order to keep track which wallet performed certain action
              Start Wallet
            -- Try to second wallet setPrice for contract of the first wallet (must not be possible)
            | SetPrice Wallet Wallet Integer
            | AddTokens Wallet Wallet Integer
            | Withdraw Wallet Wallet Integer Integer
            | BuyTokens Wallet Wallet Integer
        deriving (Show, Eq)

    -- To identify each running instance of the contract
    -- Generalized algebraic data types: used this because we need different type parameters for constructors
    data ContractInstanceKey TSModel w s e where
        -- Constructor StarKey receives one argument: Wallet, result in ContractInstanceKey
        -- Parameters for ContractInstance: TSModel: model defined, TokenSale: state type, TSStartSchema and Text: error type
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema   Text

    -- Implements String class
    -- OverlaodedStrings allow to use strings literals to define ByteStrings or TokenName or CurrencySymbol. Those types implement isString
    -- There is a default instanceTag but only allow one contract instance by wallet
    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    -- To generate an arbitrary action. Parameter: model state
    arbitraryAction _ = oneof $
        -- Using normal style: in a `do` block
        -- 
        -- w1           <-  genWallet
        -- w2           <-  genWallet
        -- genNonNeg    <-  genWallet
        -- SetPrice w1 w2 p
        -- 
        -- Applicative style: <$>
        -- First run genWallet action (to get a random wallet) then run Start action
        (Start <$> genWallet) :
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    -- Initially there is no TokenSale
    -- It's a map. Check line -> newtype TSModel is a map between Wallet and TSState
    initialState = TSModel Map.empty

    -- http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html#v:nextState
    -- Given an action, nextState specifies the effects running that action has on the model state. It runs in the Spec monad, which is a state monad over the ModelState.
    nextState (Start w) = do
        -- This operator $= comes from Spec Monad
        -- Takes a lens from Model in the left side and set the focus of that lens to the entry on the right side
        -- at: allows to find a key inside the map with target type Maybe
            -- if Just has value, then set the key on the map
            -- else Just has Nothing, then delete the key on the map
        (tsModel . at w) $= Just (TSState 0 0 0)
        -- This also comes from Spec Monad
        wait 1

    nextState (SetPrice v w p) = do
        when (v == w) $
        -- ix: traversal.
            -- If the key is there, will work with key
            -- else won't focus on anything
            -- For example: if wallet hasn't started, then nothing happens
            (tsModel . ix v . tssPrice) $= p
        wait 1

    nextState (AddTokens v w n) = do
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            -- view: similar to carry operator for zoom (.)
            -- how much change of the wallet
            bc <- askModelState $ view $ balanceChange w
            -- get the token that wallet v is selling
            let token = tokens Map.! v
            -- check how many tokens has wallet W and validate if it's greather than amount that wallet W wants to add
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                -- withdraw from wallet W (those funds just dissapear from wallet W)
                withdraw w $ assetClassValue token n
                -- Setter' $~, applies that function
                -- http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html#v:-36--126-
                -- Updates tsModel, increasing value of tssToken
                (tsModel . ix v . tssToken) $~ (+ n)
        wait 1

    nextState (BuyTokens v w n) = do
        when (n > 0) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
        wait 1

    nextState (Withdraw v w n l) = do
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
        wait 1

    -- Link between model and endpoints: how to run the operations in the emulator
    -- http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html#v:perform
    perform h _ cmd = case cmd of
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (tokenCurrencies Map.! w, tokenNames Map.! w, False) >> delay 1
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                    >> delay 1
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                    >> delay 1
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                               >> delay 1

    -- Preconditions for every action
    -- http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html#v:precondition
    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v

-- ContractInstanceKey is a GADT (General algebraic data type)
-- Sometimes deriving GADT doesn't work, so instead of writing deriving, we use StandaloneDeriving
deriving instance Eq (ContractInstanceKey TSModel w s e)
deriving instance Show (ContractInstanceKey TSModel w s e)

getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
-- contractState: Lens for the contract-specific part of the model state
-- returns TSState for Wallet v
getTSState' s v = s ^. contractState . tsModel . at v

getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    -- Get the current model state: The ModelState models the state of the blockchain.
    -- http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html#t:ModelState
    s <- getModelState
    return $ getTSState' s v

hasStarted :: Wallet -> Spec TSModel Bool
-- Just find if there is a state or not
hasStarted v = isJust <$> getTSState v

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
-- fromList [(Wallet 1,(aa,"A")),(Wallet 2,(bb,"B"))]
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

-- Create a token sale for each wallet
tss :: Map Wallet TokenSale
tss = Map.fromList
    [ (w, TokenSale { tsSeller = pubKeyHash $ walletPubKey w
                    -- looks for correct token on sale for wallet
                    , tsToken  = tokens Map.! w
                    , tsTT     = Nothing
                    })
    | w <- wallets
    ]

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

-- http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html#t:ContractInstanceSpec
-- A ContractInstanceSpec associates a ContractInstanceKey with a concrete Wallet and Contract. 
instanceSpec :: [ContractInstanceSpec TSModel]
instanceSpec =
    [ContractInstanceSpec (StartKey w) w startEndpoint | w <- wallets] ++
    [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

-- Generate a random wallet
genWallet :: Gen Wallet
-- elements: take a list and pick a random element
genWallet = elements wallets

genNonNeg :: Gen Integer
-- getNonNegative is part of QuickCheck library
genNonNeg = getNonNegative <$> arbitrary

tokenAmt :: Integer
tokenAmt = 1_000

prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d) def def)
    instanceSpec
    (const $ pure True) -- TracePredicate, True means not additional checks
  where
    d :: InitialDistribution
    -- Set the initial distribution: 1000 ADA and 1000 Token amount of both tokens
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_TS

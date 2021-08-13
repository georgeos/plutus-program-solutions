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

module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer       -- Amount stake in the game by each player
    , gPlayDeadline   :: !POSIXTime     -- Time when second player mades a move
    , gRevealDeadline :: !POSIXTime     -- Time for first player claim victory by revealing his nonce
    , gToken          :: !AssetClass    -- An NFT to identify the correct script
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    -- Inlinable pragma for equals operator
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

-- ByteString is the hash of the first player submits
-- Gamechoice is the move of the second player. It's Maybe because in the beginning second player hasn't move
data GameDatum = GameDatum ByteString (Maybe GameChoice)
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

-- Play when second player moves. Parameter: Zero || One
-- Reveal when first player has won and reveal his nonce. Parameter: nonce
-- ClaimFirst when second player doesn't play and first player claim his funds
-- ClaimSecond when first player doesn't reveal and second player claim his funds
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

-- Extract lovelace contained in a given value 
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- Get a GameDatum given a transaction output and a datum hash
{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- Game: define properties for the game
-- ByteString bsZero' bsOne': represent one and zero digits choice. Because it's not possible String literals, then it's needed pass as auxiliary arguments.

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        -- When second player is moving. Datum must be nothing because second player hasn't moved
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        -- When both players have moved and first player is revealing his nonce
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- When first player wants his stake back because second player hasn't moved at deadline
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- When both players have moved but first player hasn't revealed at deadline
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        _                              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    -- This is for UTXO after current transaction
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    outputDatum :: GameDatum
    outputDatum = case gameDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    -- Used to checkNonce if first player won
    -- bs: hash submitted by first player
    -- nonce: nonce first player reveals
    -- cSeconde: move that both players made (cause at the end, cFirst and CSecond are the same)
    checkNonce :: ByteString -> ByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `concatenate` cFirst) == bs
      where
        cFirst :: ByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    -- Check if the first player has the token (1 NFT)
    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1

data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- OFFCHAIN

-- Find the correct UTXO and Datum (output of the previous transaction), which has the NFT.
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    utxos <- utxoAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    -- Find UTXO with NFT
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1

-- Wait until time has passed and script is in the next slot
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    -- awaitTime provided by Contract monad
    -- Wait until POSIXTime and wait 1 slot
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Two contracts for two players

-- First contract for first player
firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        -- Put stake and NFT
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        c    = fpChoice fp
        -- Create hash of first player move (nonce + choice)
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
        -- Produce script output at this address with GameDatum as Datum and value computed
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    -- Waint until deadline to second player has reached
    waitUntilTimeHasPassed $ fpPlayDeadline fp

    -- Check if the UTXO contains NFT (NFT shouldn't disappear because second player only should move, not anything else)
    
    -- Find UTXO of the Game and Datum in m
    m   <- findGameOutput game
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            -- When second player didn't move
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                -- Need to provide UTXO and validator of the game
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    -- Pay to the first player from script
                    -- Redeemer ClaimFirst and validation from now 
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            -- When second player move and lost
            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    -- If first player win, then should reveal his nonce and submit it before deadline
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            -- When second player moved and won
            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Second contract for second player
secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }

    -- Find UTXO of the Game and Datum in m
    m <- findGameOutput game
    case m of
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            -- NFT should be contained in the resultant UTXO
            let token   = assetClassValue (gToken game) 1
            -- Also should contain Stake * 2 (and NFT)
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)
                -- To spend the current UTXO: mustSpendScriptOutput
                -- Send redeemer 
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>
                -- Send datum including first player move and second player move. Additional to (v) stake and NFT
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>
                        --   To validate if second player move is before deadline
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            waitUntilTimeHasPassed $ spRevealDeadline sp

            m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                -- If there is not an UTXO, means first player reveal and won
                Nothing             -> logInfo @String "first player won"
                -- If there is the same UTXO, means first player didn't reveal
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)
                        -- Spent UTXO found
                        -- Send redeemer ClaimSecond
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                   -- After (now') reveal deadline has passed
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   --    Send NFT to first player
                                   Constraints.mustPayToPubKey (spFirst sp) token
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"
        -- If there is not any game to play
        _ -> logInfo @String "no running game found"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract () GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame

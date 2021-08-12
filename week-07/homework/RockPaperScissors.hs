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

module Week07.RockPaperScissors
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken
    , Text
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

data GameChoice = Rock | Paper | Scissor
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Rock    == Rock     = True
    Paper   == Paper    = True
    Scissor == Scissor  = True
    _       == _        = False

PlutusTx.unstableMakeIsData ''GameChoice

{-# INLINABLE beats #-}
beats :: GameChoice -> GameChoice -> Bool
beats Rock      Scissor = True
beats Paper     Rock    = True
beats Scissor   Paper   = True
beats _         _       = False

data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal ByteString GameChoice | ClaimFirst | ClaimSecond | WithdrawFirst ByteString GameChoice | WithdrawSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe(TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)       -- When second player moves
        | lovelaces v == gStake game        -> Just(    Constraints.mustBeSignedBy (gSecond game) <>
                                                        Constraints.mustValidateIn (to $ gPlayDeadline game),
                                                        State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                    )
    (v, GameDatum _ (Just _), Reveal _ _)  -- When first player reveals
        | lovelaces v == 2 * gStake game    -> Just(    Constraints.mustBeSignedBy (gFirst game) <>
                                                        Constraints.mustValidateIn (to $ gRevealDeadline game),
                                                        State Finished mempty
                                                    )
    (v, GameDatum _ Nothing, ClaimFirst)    -- When first player claims by a victory
        | lovelaces v == gStake game        -> Just(    Constraints.mustBeSignedBy (gFirst game) <>
                                                        Constraints.mustValidateIn (from $ 1 + gPlayDeadline game),
                                                        State Finished mempty
                                                    )
    (v, GameDatum _ (Just _), ClaimSecond)  -- When second player claims by a victory
        | lovelaces v == 2 * gStake game    -> Just(    Constraints.mustBeSignedBy (gSecond game) <>
                                                        Constraints.mustValidateIn (from $ 1 + gRevealDeadline game),
                                                        State Finished mempty
                                                    )
    (v, GameDatum _ (Just _), WithdrawFirst _ c )   -- When first player withdraws
        | lovelaces v == 2 * gStake game    -> Just(    Constraints.mustBeSignedBy (gFirst game) <>
                                                        Constraints.mustValidateIn (to $ gRevealDeadline game),
                                                        State (GameDatum emptyByteString $ Just c) (lovelaceValueOf $ gStake game)
                                                    )
    (v, GameDatum _ (Just _), WithdrawSecond)         -- When second player withdraws
        | lovelaces v == gStake game        -> Just(    Constraints.mustBeSignedBy (gSecond game) <>
                                                        Constraints.mustValidateIn (from $ 1 + gRevealDeadline game),
                                                        State Finished mempty
                                                    )
    _                                        -> Nothing

{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE check #-}
check :: ByteString -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsRock' bsScissor' bsPaper' (GameDatum bs (Just _)) (Reveal nonce choice) _ =
    sha2_256 (nonce `concatenate`
        case choice of
            Rock    -> bsRock'
            Scissor -> bsScissor'
            _       -> bsPaper'
    ) == bs
check bsRock' bsScissor' bsPaper' (GameDatum bs (Just _)) (WithdrawFirst nonce choice) _ =
    sha2_256 (nonce `concatenate`
        case choice of
            Rock    -> bsRock'
            Scissor -> bsScissor'
            _       -> bsPaper'
    ) == bs
check _       _      _                       _              _   _ = True

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> ByteString -> ByteString -> ByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsRock' bsScissor' bsPaper' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsRock' bsScissor' bsPaper'
    , smThreadToken = Just $ gToken game
    }

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsRock' bsScissor' bsPaper' = mkValidator $ gameStateMachine game bsRock' bsScissor' bsPaper'

type Gaming = StateMachine GameDatum GameRedeemer

bsRock, bsScissor, bsPaper :: ByteString
bsRock      = "R"
bsScissor   = "S"
bsPaper     = "P"

gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsRock bsScissor bsPaper

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsRock
        `PlutusTx.applyCode` PlutusTx.liftCode bsScissor
        `PlutusTx.applyCode` PlutusTx.liftCode bsPaper)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- OFFCHAIN CODE

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- mapError' getThreadToken
    let game   = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = tt
            }
        client = gameClient game
        v      = lovelaceValueOf (fpStake fp)
        c      = fpChoice fp
        bs     = sha2_256 $ fpNonce fp `concatenate`
            case c of
                Rock    -> bsRock
                Scissor -> bsScissor
                _       -> bsPaper
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
    logInfo @String $ "made first move: " ++ show (fpChoice fp)
    tell $ Last $ Just tt

    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "game output not found"
        Just ((o, _), _) -> case tyTxOutData o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c') | c `beats` c' -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal (fpNonce fp) c
                logInfo @String "first player revealed and won"

            GameDatum _ (Just c') | c' `beats` c -> do
                logInfo @String "second player won"

            _ -> do
                logInfo @String "second player tied"
                void $ mapError' $ runStep client $ WithdrawFirst (fpNonce fp) c
                logInfo @String "first player claimed by tie"                         

data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = spToken sp
            }
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        Just ((o, _), _) -> case tyTxOutData o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                logInfo @String $ "made second move: " ++ show (spChoice sp)
                void $ mapError' $ runStep client $ Play $ spChoice sp

                waitUntilTimeHasPassed $ spRevealDeadline sp

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing             -> logInfo @String "first player won"

                    Just ((d, _), _)    -> case tyTxOutData d of

                        GameDatum bs (Just _) | bs == emptyByteString -> do
                            logInfo @String "tie found"
                            void $ mapError' $ runStep client WithdrawSecond
                            logInfo @String "second player claimed by tie"

                        _ -> do
                            logInfo @String "first player didn't claim and didn't reveal"
                            void $ mapError' $ runStep client ClaimSecond
                            logInfo @String "second player won"

            _ -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame

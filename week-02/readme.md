# Plutus

Validator script should be converted into Plutus core using ```Plutus.compile```.
It converts the validator into Plutus core.

    mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

For every helper function used in the mkValidator, we should use INLINABLE pragma. For example:

    {-# INLINABLE mkValidator #-}
    mkValidator :: Data -> Data -> Data -> ()

To check Plutus core, from ```cabal repl```:

    validator
    :i Validator
    getValidator validator
    :i Script
    unScript $ getValidator validator

To get the validator Hash:

    valHash :: Ledger.ValidatorHash
    valHash = Scripts.validatorHash validator

To get the real address on the blockchain for the validator:
http://localhost:8081/plutus-haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Address.html#t:Address

    scrAddress :: Ledger.Address
    scrAddress = scriptAddress validator

Difference between validator hash and the address, it's because address can contain staking information.

## Constraints

- mustPayToOtherScript
http://localhost:8081/plutus-haddock/plutus-ledger/html/Ledger-Constraints.html#t:TxConstraint

When a wallet constructs a transaction, we should indicate where to find all utxos and (in case of spending transaction) we should provide the actual validator code. (Producing transaction only has to provide the hash). In case of "grab" is a spending transaction.

    Constraints.unspentOutputs utxos      <>
    Constraints.otherScript validator

## Submit transactions

http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Request.html#g:9

## Implicit prelude

If we don't need to import all Haskell prelude by default, we should use a GHC extension:

    {-# LANGUAGE NoImplicitPrelude   #-}

## Overloaded strings
Strings represented by "" in Plutus are array of characters [Char]. In order to use them as strings, we should use GHC extension:

    {-# LANGUAGE OverloadedStrings   #-}

## Instance of new type
If we define a new type for IsData, we should mark it as unstable:

    PlutusTx.unstableMakeIsData ''MySillyRedeemer

Syntax is: provide type using '' in front. Using this, at compile time, it will write an instance for IsData for this type.

- unstableMakeIsData: does not make any guarantees that between different plutus version, the constructor number corresponding to a given constructor will be preserved.
- stable: you can explicitly specify which constructor corresponds to which index. So then it's guaranteed to work between different versions of Plutus.
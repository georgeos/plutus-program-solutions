# Notes
```
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Ada
```

### Activate OverloadedStrings extension
```
:set -XOverloadedStrings
```

### To convert from Integer into Value
```
:t lovelaceValueOf
lovelaceValueOf 123
```

### To combine values
```
lovelaceValueOf 123 <> lovelaceValueOf 200
```

### Create value containing native tokens
```
:t singleton
singleton "a8ff" "ABC" 7
singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
```

### Get value of native tokens
```
:t valueOf
valueOf v "a8ff" "XYZ"
:t flattenValue
flattenValue v
txInfoForge :: Value
```

### Additional
- Validation script: Datum, Redeemer and Context
- Minting policies script: Redeemer, Context (because Datum sits on something that is spent)
- Added to .cabal file on ```build-depends: bytestring ^>=0.10.12.0```

## fmap

    :t fmap
    map (+ 1) [1 :: Int, 2, 3]
    fmap (+ 1) [1 :: Int, 2, 3]
    (+ 1) <$> [1 :: Int, 2, 3]

    do
        pk <- Contract.ownPubKey
        let pkh = pubKeyHash pk

    do
        pkh <- pubKeyHash <$> Contract.ownPubKey

Explanation

    :t fmap
    -- f => (a -> b) -> f a -> f b
    -- Contract => (PubKey -> PubKeyHash) -> Contract PubKey -> Contract PubKeyHash
    -- Contract => function pubKeyHash -> pubKey -> pubKeyHash
    fmap pubKeyHash Contract.ownPubKey
    pubKeyHash <$> Contract.ownPubKey

## any
Returns true if any satisfies the condition

    any even [2 :: Int, 3, 5]
# Tests

## Test.Tasty

https://hackage.haskell.org/package/tasty

    import Test.Tasty
    main = defaultMain tests
    tests :: TestTree
    tests = testGroup "Tests" [properties, unitTests]

It uses TestTree which is provided by Plutus.Contract.Test

## Plutus.Contract.Test
http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test.html

It's the easiest way to write automatic tests for Plutus Contracts.
- Simply write one emulator trace
- Use predicate in combination with appropiate test predicates to check that those emulator traces lead to the desired result.

To execute a test:

    defaultMain tests

### Checking predicates

http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test.html#g:2

### TracePredicate
http://localhost:8081/plutus-haddock/plutus-contract/html/Plutus-Contract-Test.html#t:TracePredicate

#### Functions
- walletFundsChange: Check that the funds in the wallet have changed by the given amount, exluding fees.
- walletFundsExactChange: Check that the funds in the wallet have changed by the given amount, including fees.

## Optics: lens
- Reaching deeply into hierarchical data types.
- When working with lens, just as convention: call fields with leading underscore

https://hackage.haskell.org/package/lens

To inspect what code template haskell writes at compiles time: https://youtu.be/zW3D2iM5uVg?t=2905

    :l src/Week-08/Lens.hs
    :set -ddump-splices
    :r
    import Control.Lens

To see a value use ^.

    lars ^. name
    -- Chain lens together if they are compatibles
    -- To combine them, just use .
    lars ^. address . city

To change a value use &. It creates a new person with value updated:

    lars & name .~ "LARS"
    lars & address . city .~ "Munich"

Travesibles for each item in a list:

    [1 :: Int, 3, 4] & each .~ 42

Various types of lens can be combinated by the dot (.) operator

    iohk & staff . each . address . city .~ "Guatemala"

## Property based testing with QuickCheck
Generate random arguments for the function. By default try 100 of different random arguments, but can be configured.

    import Test.QuickCheck
    quickCheck prop_simple

In this case, quickCheck generate different random list of integers, if not passed, quickCheck reports it

    quickCheck prop_sort_sorts

For example, in this case, generate 7 lists of random integers and found one with an error. Then makes shrinks to simplify the list with error to find a simpler list of integers with error: [0,0,-1].

    *** Failed! Falsified (after 7 tests and 3 shrinks):    
    [0,0,-1]

To test, QuickCheck uses arbitrary:

    sample (arbitrary :: Gen [Int])
    :i Arbitrary

    sample (arbitraryAction undefined :: Gen (Action TSModel))

Takes an action and returns a Spec monad. Allow to inspect our current state of the model and trasfer funds in our model.

    :t nextState

### Maps

    import Control.Lens
    import qualified Data.Map.Strict as Map
    m = Map.fromList [("Haskell", True), ("Java", False)]

To see a value:

    m ^. at "Haskell"
    m ^. at ""

To set a value:

- at: returns nothing if there is not the key

```
    m & at "Python" .~ Just False (create the key)
    m & at "Haskell" .~ Just False (update the key)
    m & at "Haskell" .~ Nothing (remove the key)
```

- ix: it's a traversal. if key exists, then change it else nothing happens
```
    m & ix "Haskell" .~ False (update the key)
    m & ix "Python" .~ False (nothing happens)
```

### ModelState

    :i askModelState
    :i ModelState

### Run test

To get scenarios

    import Test.QuickCheck
    import Plutus.Contract.Test.ContractModel
    sample (arbitrary :: Gen (Actions TSModel ))

To run:

    test

# General notes Limitations using QuickCheck:

- This use off chain code provided by the contract in the perform method.
- Concurrency. In the example was used a delay 1

https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-Test.html#v:dataAtAddress

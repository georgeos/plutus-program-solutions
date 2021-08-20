{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract "alice" "bob" "charlie" 10

contract :: Party -> Party -> Party -> Integer -> Contract
contract alice bob charlie amount = 
    When
        [Case (deposit charlie $ amount * 2
            )
            (When
                [Case (deposit alice amount)
                    (When
                        [Case (deposit bob amount)
                            (When
                                [Case (Choice (choiceId charlie) [Bound 1 2])
                                    (If
                                        (ValueEQ
                                            (ChoiceValue (choiceId charlie))
                                            (Constant 1)
                                        )
                                        (payment bob alice amount
                                            (payment charlie charlie (amount * 2) Close)
                                        )
                                        (payment alice bob amount 
                                            (payment charlie charlie (amount * 2) Close)
                                        )
                                    )]
                                40
                                (payment charlie alice amount
                                    (payment charlie bob amount Close)
                                )
                            )]
                        30 Close 
                    )]
                20 Close 
            )]
        10 Close
    where
        payment :: Party -> Party -> Integer -> Contract -> Contract
        payment fromAccount toAccount amount contract = 
            Pay
                fromAccount
                (Account toAccount)
                ada
                (Constant amount)
                contract

        deposit :: Party -> Integer -> Action
        deposit account amount =
            Deposit
                account
                account
                ada
                (Constant amount)

        choiceId :: Party -> ChoiceId
        choiceId charlie = ChoiceId "Winner" charlie
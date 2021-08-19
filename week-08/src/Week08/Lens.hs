{-# LANGUAGE TemplateHaskell #-}

module Week08.Lens where

import Control.Lens

newtype Company = Company {_staff :: [Person]} deriving Show


data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show

alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    -- Use record at update sintax: to only change address of p
    movePerson p = p {_address = (_address p) {_city = there}}

-- Creates a structure similar to java: Person.Address.City
-- Fields will have same name but without underscore
makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there

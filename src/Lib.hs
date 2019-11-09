{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data Person = Person
  {
    firstName :: Text,
    lastName :: Text,
    age :: Int
  } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

jason = show $ encode (Person "Jason" "Derulo" 50)

someFunc :: IO ()
someFunc = putStrLn jason

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( botPost
    ) where

import Data.Text (Text)
import Yesod
import GHC.Generics

data Person = Person
  {
    firstName :: Text,
    lastName :: Text,
    age :: Int
  } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

botPost :: Person
botPost = Person "Jason" "Derulo" 50

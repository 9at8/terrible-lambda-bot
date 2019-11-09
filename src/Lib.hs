{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( Update
    , handleUpdate
    ) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data Update = Update
  {
    update_id :: Int,
    message :: Maybe Message
  } deriving (Show, Generic)

instance FromJSON Update
instance ToJSON Update

data Message = Message
  {
    message_id :: Int,
    chat :: Chat,
    text :: Maybe Text,
    reply_to_message :: Maybe Message
  } deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

data Chat = Chat
  {
    id :: Int
  } deriving (Show, Generic)

instance FromJSON Chat
instance ToJSON Chat

handleUpdate :: Update -> IO ()
handleUpdate u = do
  print u

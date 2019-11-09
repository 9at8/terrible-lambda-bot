{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Data.Text (Text)
import Yesod
import Lib

data App = App

mkYesod "App" [parseRoutes|
/ BotR POST
|]

instance Yesod App

postBotR :: Handler Value
postBotR = returnJson $ botPost

main :: IO ()
main = warp 3000 App

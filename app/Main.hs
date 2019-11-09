{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Data.Text (Text)
import Yesod
import qualified Lib as L

data App = App

mkYesod "App" [parseRoutes|
/ BotR POST
|]

instance Yesod App

postBotR :: Handler Value
postBotR = do
  update <- requireCheckJsonBody :: Handler L.Update
  liftIO $ L.handleUpdate update
  returnJson $ object []

main :: IO ()
main = warp 3000 App

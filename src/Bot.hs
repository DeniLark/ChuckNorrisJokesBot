{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Bot.ApiHandler (apiRunner)
import Bot.Effect (effGetCategories)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Telegram.Bot.API.GettingUpdates
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

newtype Model = Model {modelManager :: Manager}

data Action
    = Start
    | GetCategories
    | Echo Text

botApp :: IO (BotApp Model Action)
botApp = do
    m <- newManager tlsManagerSettings
    pure $
        BotApp
            { botInitialModel = Model m
            , botAction = flip updateToAction
            , botHandler = flip handleAction
            , botJobs = []
            }

handleAction :: Model -> Action -> Eff Action Model
handleAction model = \case
    GetCategories ->
        model <# do
            liftIO $ do
                categories <- apiRunner (modelManager model) effGetCategories
                mapM_ print categories
            pure ()
    Start -> do
        model
            <# do
                reply (toReplyMessage "Start message")
    Echo msg -> model <# reply (toReplyMessage msg)

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ =
    parseUpdate $
        (Start <$ command "start")
            <|> GetCategories <$ command "categories"
            <|> (Echo <$> plainText)

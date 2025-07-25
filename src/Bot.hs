{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Api (Joke (value))
import Bot.ApiHandler (apiRunner)
import Bot.Effect (effGetCategories, effGetCategoryJoke)
import Bot.Keyboard (categoriesToBot)
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
    | GeCategoryJoke Text

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
        model
            <# do
                eithCategories <- liftIO $ apiRunner (modelManager model) effGetCategories
                case eithCategories of
                    Left _ -> pure () -- !!
                    Right cats -> categoriesToBot cats

                pure ()
    Start ->
        model <# reply (toReplyMessage "Start message")
    GeCategoryJoke msg ->
        model <# do
            eithCategory <-
                liftIO $
                    apiRunner (modelManager model) $
                        effGetCategoryJoke msg
            case eithCategory of
                Left _ -> pure () -- !!
                Right cats -> reply (toReplyMessage (value cats))

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ =
    parseUpdate $
        (Start <$ command "start")
            <|> GetCategories <$ command "categories"
            <|> (GeCategoryJoke <$> plainText)

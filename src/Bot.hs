{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import System.Environment (getEnv)
import Telegram.Bot.API (Token (Token), defaultTelegramClientEnv)
import Telegram.Bot.API.GettingUpdates
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

-- runBot :: IO ()
-- runBot = do
--     token <- Token . pack <$> getEnv "CHUCK_NORRIS_JOKES_BOT"
--     env <- defaultTelegramClientEnv token
--     putStrLn "Bot is running"
--     _ <- startBot bot env
--     pure ()

type Model = ()

data Action
    = Start
    | Echo Text

botApp :: BotApp Model Action
botApp =
    BotApp
        { botInitialModel = ()
        , botAction = flip updateToAction
        , botHandler = flip handleAction
        , botJobs = []
        }

handleAction :: Model -> Action -> Eff Action ()
handleAction model = \case
    Start -> model <# reply (toReplyMessage "Start message")
    Echo msg -> model <# reply (toReplyMessage msg)

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ =
    parseUpdate $
        (Start <$ command "start")
            <|> (Echo <$> plainText)

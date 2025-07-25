{-# LANGUAGE OverloadedStrings #-}

module Bot.Keyboard where

import Data.Text (Text)
import Telegram.Bot.API
import Telegram.Bot.Simple (BotM, ReplyMessage (..), reply, toReplyMessage)

createKeyboardButton :: Text -> KeyboardButton
createKeyboardButton text =
    KeyboardButton
        { keyboardButtonText = text
        , keyboardButtonRequestUsers = Nothing
        , keyboardButtonRequestChat = Nothing
        , keyboardButtonRequestContact = Nothing
        , keyboardButtonRequestLocation = Nothing
        , keyboardButtonRequestPoll = Nothing
        , keyboardButtonWebApp = Nothing
        }

categoriesToKeyboard :: [Text] -> ReplyKeyboardMarkup
categoriesToKeyboard categories =
    ReplyKeyboardMarkup
        { replyKeyboardMarkupKeyboard =
            map (pure . createKeyboardButton) categories
        , replyKeyboardMarkupIsPersistent = Just False
        , replyKeyboardMarkupResizeKeyboard = Just True
        , replyKeyboardMarkupOneTimeKeyboard = Just True
        , replyKeyboardMarkupSelective = Just True
        , replyKeyboardMarkupInputFieldSelector = Nothing
        }

categoriesToBot :: [Text] -> BotM ()
categoriesToBot categories =
    reply
        (toReplyMessage "Choose category:")
            { replyMessageReplyMarkup =
                Just $
                    SomeReplyKeyboardMarkup $
                        categoriesToKeyboard categories
            }
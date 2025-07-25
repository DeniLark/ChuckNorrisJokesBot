{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Run (runBot) where

import Bot.AppHandler

runBot :: IO ()
runBot = do
    putStrLn "Bot running"
    appRunner appAction >>= \case
        Left e -> print e
        Right _ -> pure ()
    putStrLn "After bot"

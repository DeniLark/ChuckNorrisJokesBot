{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Effect where

import Api
import Effectful (Effect)
import Effectful.TH (makeEffect)
import Servant.Client (ClientEnv)
import Telegram.Bot.Simple (BotApp)

data AppEff :: Effect where
    EffGetClientEnv :: AppEff m ClientEnv
    EffRunBot :: BotApp a b -> ClientEnv -> AppEff m ()

data ApiEff :: Effect where
    EffGetCategories :: ApiEff m [Category]
    EffGetRandomJoke :: ApiEff m Joke
    EffGetCategoryJoke :: Category -> ApiEff m Joke

makeEffect ''AppEff
makeEffect ''ApiEff
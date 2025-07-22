{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Effect where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Servant.Client (ClientEnv)
import Telegram.Bot.Simple (BotApp)

data AppEff :: Effect where
    EffGetClientEnv :: AppEff m ClientEnv
    EffRunBot :: BotApp a b -> ClientEnv -> AppEff m ()

makeEffect ''AppEff
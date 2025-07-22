{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Bot.AppHandler where

import Bot (botApp)
import Bot.Effect (AppEff (..), effGetClientEnv, effRunBot)
import Bot.Error (AppError (AppClientError, AppIOException))
import Data.Text (pack)
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Environment (Environment, getEnv, runEnvironment)
import Effectful.Error.Dynamic (CallStack)
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.Exception (catchIO)
import Telegram.Bot.API (Token (..))
import Telegram.Bot.API.MakingRequests (defaultTelegramClientEnv)
import Telegram.Bot.Simple (startBot)

runAppIO ::
    ( IOE :> es
    , Environment :> es
    , Error AppError :> es
    ) =>
    Eff (AppEff : es) a ->
    Eff es a
runAppIO = interpret_ $ \case
    EffGetClientEnv -> do
        let act = do
                token <- Token . pack <$> getEnv "CHUCK_NORRIS_JOKES_BOT"
                liftIO $ defaultTelegramClientEnv token
        act `catchIO` (throwError . AppIOException)
    EffRunBot bot env ->
        liftIO (startBot bot env)
            >>= \case
                Left e -> throwError $ AppClientError e
                Right _ -> pure ()

appAction :: (AppEff :> es, IOE :> es, Error AppError :> es) => Eff es ()
appAction = effGetClientEnv >>= effRunBot botApp

appRunner ::
    Eff '[AppEff, Error AppError, Environment, IOE] a ->
    IO (Either (CallStack, AppError) a)
appRunner = runEff . runEnvironment . runError @AppError . runAppIO

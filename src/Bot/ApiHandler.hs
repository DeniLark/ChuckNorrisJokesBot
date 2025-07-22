{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Bot.ApiHandler where

import Api (getCategories, getCategoryJoke, getRandomJoke)
import Bot.Effect (ApiEff (..))
import Bot.Error (AppError (AppClientError))
import Effectful
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Error.Dynamic (Error, runError, throwError)
import Effectful.Error.Static (CallStack)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Https), mkClientEnv, runClientM)

runApiIO :: (IOE :> es, Error AppError :> es) => Manager -> Eff (ApiEff : es) a -> Eff es a
runApiIO m = interpret_ $ \case
    EffGetCategories -> sendRequest m getCategories
    EffGetRandomJoke -> sendRequest m getRandomJoke
    EffGetCategoryJoke category -> sendRequest m $ getCategoryJoke $ Just category

apiRunner ::
    Manager ->
    Eff '[ApiEff, Error AppError, IOE] a ->
    IO (Either (CallStack, AppError) a)
apiRunner m = liftIO . runEff . runError @AppError . runApiIO m

sendRequest ::
    (IOE :> es, Error AppError :> es) =>
    Manager ->
    ClientM a ->
    Eff es a
sendRequest m client = do
    result <-
        liftIO $ runClientM client (mkClientEnv m (BaseUrl Https "api.chucknorris.io" 443 ""))
    case result of
        Left e -> throwError $ AppClientError e
        Right a -> pure a

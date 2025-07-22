{-# LANGUAGE DeriveGeneric #-}

module Api where

import Data.Aeson
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client (ClientM, client)

newtype Joke = Joke {value :: Text}
    deriving (Show, Generic)

instance FromJSON Joke

type Category = Text

type API =
    "jokes" :> "random" :> Get '[JSON] Joke
        :<|> "jokes" :> "categories" :> Get '[JSON] [Category]
        :<|> "jokes" :> "random" :> QueryParam "category" Text :> Get '[JSON] Joke

api :: Proxy API
api = Proxy

getRandomJoke :: ClientM Joke
getCategories :: ClientM [Category]
getCategoryJoke :: Maybe Category -> ClientM Joke
getRandomJoke :<|> getCategories :<|> getCategoryJoke = client api
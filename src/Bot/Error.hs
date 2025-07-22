module Bot.Error where

import Effectful.Exception (IOException)
import Servant.Client (ClientError)

data AppError
    = AppIOException IOException
    | AppClientError ClientError
    deriving (Show)
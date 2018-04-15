{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant
import Servant.Server
import Servant.Client

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger

data Event = Event { pull_request :: PullRequest }
    deriving (Generic, FromJSON)

data PullRequest = PullRequest { number :: Integer }
    deriving (Generic, FromJSON)

type WebHook =
        Header "X-GitHub-Event" Text
    :>  ReqBody '[JSON] Event
    :>  Post '[JSON] ()

data Comment = Comment { body :: Text }
    deriving (Generic, ToJSON)

data Response = Response { }
    deriving (Generic, FromJSON)

type GitHub =
        Header "User-Agent" Text
    :>  Header "Authorization" Text
    :>  "repos"
    :>  Capture "owner" Text
    :>  Capture "repo" Text
    :>  "issues"
    :>  Capture "number" Integer
    :>  "comments"
    :>  ReqBody '[JSON] Comment
    :>  Post '[JSON] Response

-- addComment
--     :: Maybe Text -> Maybe Text -> Text -> Text -> Integer -> Comment
--     -> ClientM Response
addComment :: Client GitHub
addComment = client @GitHub Proxy

main :: IO ()
main = do
    manager <- Network.HTTP.Client.newManager Network.HTTP.Client.TLS.tlsManagerSettings
    token <- fmap Data.Text.strip (Data.Text.IO.readFile "/tmp/token.txt")
    baseUrl <- Servant.Client.parseBaseUrl "https://api.github.com"
    let clientEnv = ClientEnv manager baseUrl

    let server :: Server WebHook
        server (Just "pull_request") event = do
            let userAgent = Just "Gabriel439"
            let authorization = Just ("token " <> token)
            let comment = Comment { body = "Hello!" }
            let command = addComment userAgent authorization "Gabriel439" "test" (number (pull_request event)) comment
            x <- liftIO (runClientM command clientEnv)
            case x of
                Left exception -> liftIO (Control.Exception.throwIO exception)
                Right _        -> return ()
        server _ _ = fail "Unexpected X-GitHub-Event header"


    let application :: Application
        application =
            Network.Wai.Middleware.RequestLogger.logStdoutDev (serve @WebHook Proxy server)

    Network.Wai.Handler.Warp.run 8080 application

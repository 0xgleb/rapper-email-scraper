module Server
  ( runServer
  )
  where

import qualified Bot
import qualified Scraper
import qualified Twitter

import Network.Wai.Handler.Warp
import Protolude
import Servant
-- import Servant.Server

type API
  = "free-search" :> Post '[JSON] [Scraper.Email]

server :: Server API
server = Bot.run $ Scraper.Free $ Twitter.FreeSearch Nothing

runServer :: IO ()
runServer = do
  putStrLn @Text "Starting the server on 8000"
  run 8080 $ serve (Proxy @API) server

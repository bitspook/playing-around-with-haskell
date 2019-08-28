module Lib
    ( run
    ) where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)
import qualified Zero.Server  as Server

helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

run :: IO ()
run = Server.startServer [Server.simpleHandler Server.GET "/hello" helloHandler]

module Lib
  ( run
  ) where

import           Cart
import qualified Data.Aeson      as Aeson
import qualified Data.Map.Strict as Map
import           GHC.Generics    (Generic)
import qualified Zero.Server     as Server

helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

echoHandler :: Server.Request -> Server.Response
echoHandler = Server.stringResponse . Server.requestBody

caseMatchHandler :: Server.Request -> Server.Response
caseMatchHandler req =
  case Server.requestBody req of
    "1"       -> Server.stringResponse "one"
    "2"       -> Server.stringResponse "two"
    "3"       -> Server.stringResponse "three"
    otherwise -> Server.stringResponse "What am I, a mathematician?"

stringManipHandler :: Server.Request -> Server.Response
stringManipHandler req = Server.stringResponse str_
  where
    str = Server.requestBody req
    match = "I'm positive"
    replace = "I think"
    isMatch = take (length match) str == match
    str_
      | isMatch = replace <> drop (length match) str
      | otherwise = str

data OnOffState
  = On
  | Off
  deriving (Eq, Show)

type CounterState = Int

onOffHandler :: OnOffState -> Server.Request -> (OnOffState, Server.Response)
onOffHandler On req  = (Off, Server.stringResponse (show Off))
onOffHandler Off req = (On, Server.stringResponse (show On))

increaseHandler :: CounterState -> Server.Request -> (CounterState, Server.Response)
increaseHandler n req = (n + 1, Server.stringResponse (show n))

countHandler :: CounterState -> Server.Request -> (CounterState, Server.Response)
countHandler n req = (n, Server.stringResponse (show n))

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.GET "/hello" helloHandler
    , Server.simpleHandler Server.POST "/echo" echoHandler
    , Server.simpleHandler Server.POST "/case" caseMatchHandler
    , Server.simpleHandler Server.POST "/string-manipulation" stringManipHandler
    , Server.handlersWithState
        Off
        [Server.statefulHandler Server.POST "/onoff-switch" onOffHandler]
    , Server.handlersWithState
        0
        [ Server.statefulHandler Server.POST "/increase" increaseHandler
        , Server.statefulHandler Server.GET "/current-count" countHandler
        ]
    , Server.handlersWithState
        (Cart Map.empty)
        [ Server.statefulHandler Server.POST "/cart" createCartHanler
        , Server.statefulHandler Server.GET "/cart" getCartHandler
        ]
    ]

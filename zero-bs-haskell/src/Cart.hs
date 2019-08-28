module Cart (getCartHandler, createCartHanler) where
import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)
import qualified Zero.Server  as Server

data Item = Item
  { model    :: String
  , quantity :: Int
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

type Cart = [Item]

getCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
getCartHandler cart req = (cart, Server.jsonResponse cart)

createCartHanler :: Cart -> Server.Request -> (Cart, Server.Response)
createCartHanler cart req = (newCart, Server.stringResponse msg)
  where
    item :: Either String Item
    item = Server.decodeJson $ Server.requestBody req
    newCart =
      case item of
        Left _     -> []
        Right item -> cart ++ [item]
    msg =
      case item of
        Left err   -> "Error while adding item" <> err
        Right item -> show item

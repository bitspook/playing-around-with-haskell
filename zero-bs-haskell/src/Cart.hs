module Cart
  ( Cart (..)
  , getCartHandler
  , createCartHanler
  ) where
import qualified Data.Aeson      as Aeson
import           Data.List       (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics    (Generic)
import qualified Zero.Server     as Server


data Item = Item
  { model    :: String
  , quantity :: Int
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)
data Cart =
  Cart (Map String Item)
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

getCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
getCartHandler (Cart cart) req = (Cart cart, Server.jsonResponse $ items cart)
  where
    items = reverse . sortOn quantity . Map.elems

createCartHanler :: Cart -> Server.Request -> (Cart, Server.Response)
createCartHanler (Cart cart) req = (Cart newCart, Server.stringResponse msg)
  where
    item :: Either String Item
    item = Server.decodeJson $ Server.requestBody req
    newCart =
      case item of
        Left _ -> Map.empty
        Right item ->
          case Map.lookup (model item) cart of
            Just exItem ->
              Map.insert
                (model item)
                (exItem {quantity = (quantity exItem) + (quantity item)})
                cart
            Nothing -> Map.insert (model item) item cart
    msg =
      case item of
        Left err   -> "Error while adding item" <> err
        Right item -> show item

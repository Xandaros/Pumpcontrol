{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where
import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader
import Data.Acid
import qualified Data.IntMap as Map

import Servant
import Network.URI (URIAuth(..), relativeTo, nullURI)
import Network.Wai
import Network.Wai.Handler.Warp

import API
import Control
import DB
import Types

type MyHandler = ReaderT (AcidState Schema) Handler
type MyServer api = ServerT api MyHandler

main :: IO ()
main = do
    batLow <- batteryLow
    state <- openLocalState defaultSchema
    when batLow $ do
        update state $
            SetBatteryBlock True
    finally (run 8080 (app state))
            (closeAcidState state)

app :: AcidState Schema -> Application
app state = serve (Proxy :: Proxy API) (enter (runReaderTNat state) server)

server :: MyServer API
server = pumpsServer :<|> batteryServer

-- Routing
pumpsServer :: MyServer PumpsAPI
pumpsServer = pumpsGET :<|> pumpsPOST :<|> pumpServer

pumpServer :: Int -> MyServer PumpAPI
pumpServer pumpid = _pumpGET :<|> _pumpPUT :<|> _pumpDELETE
               :<|> pumpStateServer pumpid :<|> pumpSchedulesServer pumpid

pumpStateServer :: Int -> MyServer PumpStateAPI
pumpStateServer pumpid = _pumpStateGET :<|> _pumpStatePUT

pumpSchedulesServer :: Int -> MyServer PumpSchedulesAPI
pumpSchedulesServer pumpid = _pumpSchedulesGET :<|> _pumpSchedulesPOST
                        :<|> pumpScheduleServer pumpid

pumpScheduleServer :: Int -> Int -> MyServer PumpScheduleAPI
pumpScheduleServer pumpid schedid = _pumpScheduleAPI :<|> _pumpSchedulePUT
                               :<|> _pumpScheduleDELETE

batteryServer :: MyServer BatteryAPI
batteryServer = _batteryGET :<|> _batteryPUT :<|> batteryBlockServer

batteryBlockServer :: MyServer BatteryBlockAPI
batteryBlockServer = _batteryBlockGET :<|> _batteryBlockPUT

-- Implementations
pumpsGET :: MyHandler [Pump]
pumpsGET = do
    state <- ask
    pumps <- liftIO $ query state GetPumps
    pure $ fmap snd (Map.toList pumps)

pumpsPOST :: Maybe String -> Pump -> MyHandler (PostReturn Pump)
pumpsPOST host pump = do
    state <- ask
    (ident, pump) <- liftIO $ update state (AddPump pump)
    let endpoint = Proxy :: Proxy ("pumps"
                                :> Capture "id" Int
                                :> Get '[JSON] Pump)
        uri = apiURI endpoint ident

    pure $ postReturn uri host (Just ident) pump

-- Util

apiURI :: (IsElem endpoint API, HasLink endpoint)
       => Proxy endpoint -> MkLink endpoint
apiURI = safeLink (Proxy :: Proxy API)

postReturn :: URI -> Maybe String -> Maybe Int -> a -> PostReturn a
postReturn uri host ident datas =
    PostReturn{ ident = ident
              , uri   = Just (uri `relativeTo` baseURI)
              , datas = datas
              }
    where
       baseURI = let auth = URIAuth "" <$> hostPart <*> portPart
                     hostPart = takeWhile (/=':') <$> host
                     portPart = dropWhile (/=':') <$> host
                 in  case host of
                   Nothing -> nullURI
                   Just _  -> nullURI{ uriScheme = "http:"
                                     , uriAuthority = auth
                                     }

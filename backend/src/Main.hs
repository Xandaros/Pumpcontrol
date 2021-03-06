{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where
import Control.Exception (finally)
import Control.Monad (when, (<=<))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader
import Data.Acid
import qualified Data.IntMap as Map

import Servant
import Network.URI (URIAuth(..), relativeTo, nullURI)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

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
    finally (run 8080 (simpleCors $ app state))
        (closeAcidState state)

app :: AcidState Schema -> Application
app state = serve (Proxy :: Proxy API) $
    (enter (runReaderTNat state) server) :<|> frontend

server :: MyServer Backend
server = pumpsServer :<|> batteryServer

-- Routing
pumpsServer :: MyServer PumpsAPI
pumpsServer = pumpsGET :<|> pumpsPOST :<|> pumpServer

pumpServer :: Int -> MyServer PumpAPI
pumpServer pumpid = pumpGET pumpid :<|> pumpPUT pumpid
               :<|> pumpDELETE pumpid :<|> pumpStateServer pumpid
               :<|> pumpSchedulesServer pumpid

pumpStateServer :: Int -> MyServer PumpStateAPI
pumpStateServer pumpid = pumpStateGET pumpid :<|> pumpStatePUT pumpid

pumpSchedulesServer :: Int -> MyServer PumpSchedulesAPI
pumpSchedulesServer pumpid = pumpSchedulesGET pumpid
                        :<|> pumpSchedulesPOST pumpid
                        :<|> pumpScheduleServer pumpid

pumpScheduleServer :: Int -> Int -> MyServer PumpScheduleAPI
pumpScheduleServer pumpid schedid = pumpScheduleGET pumpid schedid
                               :<|> pumpSchedulePUT pumpid schedid
                               :<|> pumpScheduleDELETE pumpid schedid

batteryServer :: MyServer BatteryAPI
batteryServer = batteryGET :<|> batteryPUT :<|> batteryBlockServer

batteryBlockServer :: MyServer BatteryBlockAPI
batteryBlockServer = batteryBlockGET :<|> batteryBlockPUT

-- Implementations

frontend :: Server Frontend
frontend = serveDirectory "../frontend/dist/build/frontend/frontend.jsexe"

pumpsGET :: Maybe String -> MyHandler ([PostReturn Pump])
pumpsGET host = do
    pumps <- queryState GetPumps
    pure . Map.elems $ Map.mapWithKey (\k p ->
        let endpoint = Proxy :: Proxy ("pumps"
                                    :> Capture "id" Int
                                    :> Get '[JSON] Pump)
            uri = apiURI endpoint k
        in  postReturn uri host (Just k) p
                          ) pumps

pumpsPOST :: Maybe String -> Pump -> MyHandler (PostReturn Pump)
pumpsPOST host pump = do
    (ident, pump) <- updateState (AddPump pump)
    updateState (SetPumpState ident Off)
    let endpoint = Proxy :: Proxy ("pumps"
                                :> Capture "id" Int
                                :> Get '[JSON] Pump)
        uri = apiURI endpoint ident

    pure $ postReturn uri host (Just ident) pump

pumpGET :: Int -> MyHandler Pump
pumpGET pump = queryState (GetPump pump) >>= \case
    Just pump -> pure pump
    Nothing -> lift $ throwE err404

pumpPUT :: Int -> Pump -> MyHandler Pump
pumpPUT ident pump = updateState (SetPump ident pump) *> pure pump

pumpDELETE :: Int -> MyHandler ()
pumpDELETE pump = updateState (DeletePump pump)

pumpStateGET :: Int -> MyHandler PumpState
pumpStateGET = maybe404 <=< queryState . GetPumpState

pumpStatePUT :: Int -> PumpState -> MyHandler PumpState
pumpStatePUT pump state = updateState (SetPumpState pump state) *> pure state

pumpSchedulesGET :: Int -> MyHandler (Map.IntMap TimeSchedule)
pumpSchedulesGET = maybe404 <=< queryState . GetPumpSchedules

pumpSchedulesPOST :: Int -> Maybe String -> TimeSchedule -> MyHandler (PostReturn TimeSchedule)
pumpSchedulesPOST pump host schedule = do
    (ident, schedule) <- updateState (AddPumpSchedule pump schedule) >>= maybe404
    let endpoint = Proxy :: Proxy ("pumps"
                                :> Capture "id" Int
                                :> "schedule"
                                :> Capture "id" Int
                                :> Get '[JSON] TimeSchedule
                                    )
        uri = apiURI endpoint pump ident
    pure $ postReturn uri host (Just ident) schedule

pumpScheduleGET :: Int -> Int -> MyHandler TimeSchedule
pumpScheduleGET pump schedule =
    queryState (GetPumpSchedule pump schedule) >>= maybe404

pumpSchedulePUT :: Int -> Int -> TimeSchedule -> MyHandler TimeSchedule
pumpSchedulePUT pumpid schedid schedule =
    updateState (SetPumpSchedule pumpid schedid schedule) *> pure schedule

pumpScheduleDELETE :: Int -> Int -> MyHandler ()
pumpScheduleDELETE pump schedule = updateState (DeletePumpSchedule pump schedule)


batteryGET :: MyHandler BatterySchedule
batteryGET = queryState GetBatterySchedule

batteryPUT :: BatterySchedule -> MyHandler BatterySchedule
batteryPUT sched = updateState (SetBatterySchedule sched) *> pure sched

batteryBlockGET :: MyHandler Bool
batteryBlockGET = queryState GetBatteryBlock

batteryBlockPUT :: Bool -> MyHandler Bool
batteryBlockPUT block = updateState (SetBatteryBlock block) *> pure block

-- Util

maybe404 :: Maybe a -> MyHandler a
maybe404 Nothing = lift $ throwE err404
maybe404 (Just x) = pure x

queryState
    :: (QueryEvent event)
    => event
    -> ReaderT (AcidState (EventState event)) Handler (EventResult event)
queryState ev = do
    state <- ask
    liftIO $ query state ev

updateState
    :: (UpdateEvent event)
    => event
    -> ReaderT (AcidState (EventState event)) Handler (EventResult event)
updateState ev = do
    state <- ask
    liftIO $ update state ev

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

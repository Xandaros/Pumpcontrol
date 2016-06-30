{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where
import qualified Data.IntMap as Map
import Servant

import Types

type API = Backend :<|> Frontend

type Backend = "pumps"   :> PumpsAPI
          :<|> "battery" :> BatteryAPI

type Frontend = Raw

type PumpsAPI = Header "Host" String :> Get '[JSON] ([PostReturn  Pump])
           :<|> Header "Host" String
             :> ReqBody '[JSON] Pump
             :> PostCreated '[JSON] (PostReturn Pump)
           :<|> Capture "id" Int :> PumpAPI

type PumpAPI = Get '[JSON] Pump
          :<|> ReqBody '[JSON] Pump :> Put '[JSON] Pump
          -- :<|> Alter
          :<|> DeleteNoContent '[JSON] ()
          :<|> "state"    :> PumpStateAPI
          :<|> "schedule" :> PumpSchedulesAPI

type PumpStateAPI = Get '[JSON] PumpState
               :<|> ReqBody '[JSON] PumpState :> Put '[JSON] PumpState --()?

type PumpSchedulesAPI = Get '[JSON] (Map.IntMap TimeSchedule)
                   :<|> Header "Host" String
                     :> ReqBody '[JSON] TimeSchedule
                     :> PostCreated '[JSON] (PostReturn TimeSchedule)
                   :<|> Capture "id" Int :> PumpScheduleAPI

type PumpScheduleAPI = Get '[JSON] TimeSchedule
                  :<|> ReqBody '[JSON] TimeSchedule :> Put '[JSON] TimeSchedule --()?
                  -- :<|> Alter
                  :<|> DeleteNoContent '[JSON] ()

type BatteryAPI = Get '[JSON] BatterySchedule
             :<|> ReqBody '[JSON] BatterySchedule :> Put '[JSON] BatterySchedule --()?
             -- :<|> Alter
             :<|> "block" :> BatteryBlockAPI

type BatteryBlockAPI = Get '[JSON] Bool
                  :<|> ReqBody '[JSON] Bool :> Put '[JSON] Bool --()?
                  -- :<|> Alter

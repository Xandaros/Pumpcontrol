{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where
import Servant

import Types

type API = "pumps"   :> PumpsAPI
      :<|> "battery" :> BatteryAPI

type PumpsAPI = Get '[JSON] [Pump]
           :<|> ReqBody '[JSON] Pump :> PostCreated '[JSON] (Int, Pump)
           :<|> Capture "id" Int :> PumpAPI

type PumpAPI = Get '[JSON] Pump
          :<|> ReqBody '[JSON] Pump :> Put '[JSON] Pump
          -- :<|> Alter
          :<|> DeleteNoContent '[JSON] ()
          :<|> "state"    :> PumpStateAPI
          :<|> "schedule" :> PumpSchedulesAPI

type PumpStateAPI = Get '[JSON] PumpState
               :<|> ReqBody '[JSON] PumpState :> Put '[JSON] PumpState --()?

type PumpSchedulesAPI = Get '[JSON] [TimeSchedule]
                   :<|> ReqBody '[JSON] TimeSchedule :> Post '[JSON] TimeSchedule
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

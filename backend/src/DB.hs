{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module DB where
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.IntMap as Map

import Data.Acid
import Data.SafeCopy
import Data.Typeable

import Types

data Schema = Schema { pumps           :: Map.IntMap Pump
                     , pumpStates      :: Map.IntMap PumpState
                     , pumpSchedules   :: Map.IntMap [TimeSchedule]
                     , batterySchedule :: BatterySchedule
                     , batteryBlock    :: Bool
                     } deriving (Typeable)

defaultSchema :: Schema
defaultSchema = Schema { pumps           = mempty
                       , pumpStates      = mempty
                       , pumpSchedules   = mempty
                       , batterySchedule = defaultBatterySchedule
                       , batteryBlock    = False
                       }

defaultBatterySchedule :: BatterySchedule
defaultBatterySchedule= BatterySchedule { lowBelow       = 30
                                         , criticalBelow = 20
                                         , highAbove     = 90
                                         }

getPumps :: Query Schema (Map.IntMap Pump)
getPumps = asks pumps

addPump :: Pump -> Update Schema (Int, Pump)
addPump pump = do
    pumps' <- gets pumps
    let key = case Map.maxViewWithKey pumps' of
                 Just ((max,_),_) -> max+1
                 Nothing          -> 0
        newmap = Map.insert key pump pumps'
    modify (\x -> x{pumps=newmap})
    pure (key, pump)

setBatteryBlock :: Bool -> Update Schema ()
setBatteryBlock val = modify $ \x -> x{batteryBlock = val}

deriveSafeCopy 0 'base ''Schema
deriveSafeCopy 0 'base ''Pump
deriveSafeCopy 0 'base ''PumpState
deriveSafeCopy 0 'base ''TimeSchedule
deriveSafeCopy 0 'base ''BatterySchedule
makeAcidic ''Schema ['getPumps, 'addPump, 'setBatteryBlock]

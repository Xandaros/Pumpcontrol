{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
                     , batterySchedule :: BatterySchedule
                     , batteryBlock    :: Bool
                     } deriving (Typeable)

defaultSchema :: Schema
defaultSchema = Schema { pumps           = mempty
                       , pumpStates      = mempty
                       , batterySchedule = defaultBatterySchedule
                       , batteryBlock    = False
                       }

defaultBatterySchedule :: BatterySchedule
defaultBatterySchedule= BatterySchedule { lowBelow       = 30
                                        , criticalBelow = 20
                                        , highAbove     = 90
                                        }

getFromIntMap :: (Schema -> Map.IntMap a) -> Int -> Query Schema (Maybe a)
getFromIntMap f key = Map.lookup key <$> asks f

getPumps :: Query Schema (Map.IntMap Pump)
getPumps = asks pumps

getPump :: Int -> Query Schema (Maybe Pump)
getPump = getFromIntMap pumps

addPump :: Pump -> Update Schema (Int, Pump)
addPump pump = do
    pumps' <- gets pumps
    let key = case Map.maxViewWithKey pumps' of
                 Just ((max,_),_) -> max+1
                 Nothing          -> 0
    setPump key pump
    pure (key, pump)

setPump :: Int -> Pump -> Update Schema ()
setPump ident pump = do
    pumps' <- gets pumps
    let newmap = Map.insert ident pump pumps'
    modify (\x -> x{pumps=newmap})

deletePump :: Int -> Update Schema ()
deletePump pump = do
    pumps' <- gets pumps
    let newPumps = Map.delete pump pumps'
    modify (\x -> x{pumps=newPumps})

getPumpState :: Int -> Query Schema (Maybe PumpState)
getPumpState = getFromIntMap pumpStates

setPumpState :: Int -> PumpState -> Update Schema ()
setPumpState ident ps = do
    pstates <- gets pumpStates
    let newStates = Map.insert ident ps pstates
    modify(\x -> x{pumpStates=newStates})

getPumpSchedules :: Int -> Query Schema (Maybe (Map.IntMap TimeSchedule))
getPumpSchedules pumpid = do
    pump <- getFromIntMap pumps pumpid
    pure $ schedules <$> pump

addPumpSchedule :: Int -> TimeSchedule -> Update Schema (Maybe (Int, TimeSchedule))
addPumpSchedule pumpid schedule = liftQuery (getPump pumpid) >>= \case
      Nothing   -> pure Nothing
      Just pump -> do
          let schedules' = schedules pump
              key = case Map.maxViewWithKey schedules' of
                       Just ((max,_),_) -> max+1
                       Nothing          -> 0
          setPumpSchedule pumpid key schedule
          pure $ Just (key, schedule)

getPumpSchedule :: Int -> Int -> Query Schema (Maybe TimeSchedule)
getPumpSchedule pumpid scheduleid = (>>= Map.lookup scheduleid) . fmap schedules <$> getPump pumpid

setPumpSchedule :: Int -> Int -> TimeSchedule -> Update Schema (Maybe ())
setPumpSchedule pumpid scheduleid schedule = liftQuery (getPump pumpid) >>= \case
    Nothing   -> pure Nothing
    Just pump -> do
        let schedules' = schedules pump
            newSched = Map.insert scheduleid schedule schedules'
        Just <$> setPump pumpid (pump{schedules=newSched})

deletePumpSchedule :: Int -> Int -> Update Schema ()
deletePumpSchedule pumpid schedid = liftQuery (getPump pumpid) >>= \case
    Nothing -> pure ()
    Just pump -> do
        let schedules' = schedules pump
            newSched = Map.delete schedid schedules'
        setPump pumpid (pump{schedules=newSched})

setBatteryBlock :: Bool -> Update Schema ()
setBatteryBlock val = modify $ \x -> x{batteryBlock = val}

deriveSafeCopy 0 'base ''Schema
deriveSafeCopy 0 'base ''Pump
deriveSafeCopy 0 'base ''PumpState
deriveSafeCopy 0 'base ''TimeSchedule
deriveSafeCopy 0 'base ''BatterySchedule
makeAcidic ''Schema [ 'getPumps
                    , 'addPump
                    , 'getPump
                    , 'setPump
                    , 'deletePump
                    , 'getPumpState
                    , 'setPumpState
                    , 'getPumpSchedules
                    , 'addPumpSchedule
                    , 'getPumpSchedule
                    , 'setPumpSchedule
                    , 'deletePumpSchedule
                    , 'setBatteryBlock
                    ]

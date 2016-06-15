{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import GHC.Generics
import Servant.Utils.Links (URI)

data Pump = Pump { name :: String
                 , description :: String
                 , gpio :: Int
                 } deriving (Generic)

instance ToJSON Pump
instance FromJSON Pump

data PumpState = Off | On | Automatic
    deriving (Generic)

instance ToJSON PumpState
instance FromJSON PumpState

data TimeSchedule = TimeSchedule { startTime :: Time
                                 , endTime :: Time
                                 , rainAbove :: Maybe Int
                                 , rainBelow :: Maybe Int
                                 , lightAbove :: Maybe Int
                                 , lightBelow :: Maybe Int
                                 } deriving (Generic)

instance ToJSON TimeSchedule
instance FromJSON TimeSchedule

type Hours = Int
type Minutes = Int
type Time = (Hours, Minutes)

data BatterySchedule = BatterySchedule { lowBelow :: Int
                                       , criticalBelow :: Int
                                       , highAbove :: Int
                                       } deriving (Generic)

instance ToJSON BatterySchedule
instance FromJSON BatterySchedule

data PostReturn a = PostReturn { ident :: Maybe Int
                               , uri :: Maybe URI
                               , datas :: a
                               } deriving (Show)

instance ToJSON a => ToJSON (PostReturn a) where
    toJSON (PostReturn{..}) = object [ "id" .= ident
                                     , "uri" .= (show <$> uri)
                                     , "data" .= datas
                                     ]

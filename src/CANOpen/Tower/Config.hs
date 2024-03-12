{-# LANGUAGE RecordWildCards #-}
module CANOpen.Tower.Config
  ( CANOpenConfig(..)
  , defaultCANOpenConfig
  , canopenConfigParser
  ) where

import Ivory.Tower.Config

data CANOpenConfig =
  CANOpenConfig {
    canOpenConfigNodeID :: Maybe Int
  } deriving (Show)

-- | By default, no ID is configured
-- and node starts LSS
defaultCANOpenConfig :: CANOpenConfig
defaultCANOpenConfig = CANOpenConfig Nothing

canopenConfigParser :: ConfigParser CANOpenConfig
canopenConfigParser = do
  let CANOpenConfig{..} = defaultCANOpenConfig

  mayNodeID <- optional $ subsection "node-id" int
  pure $ CANOpenConfig mayNodeID

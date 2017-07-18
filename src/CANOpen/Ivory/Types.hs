{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CANOpen.Ivory.Types where

import Ivory.Language

[ivory|
struct device_info
  { vendor_id :: Stored Uint32
  ; product_code :: Stored Uint32
  ; revision_number :: Stored Uint32
  ; serial_number :: Stored Uint32
  ; node_id :: Stored Uint8
  ; baudrate :: Stored Uint8
  ; rx_pdo_count :: Stored Uint8
  ; tx_pdo_count :: Stored Uint8
  ; lss_supported :: Stored IBool
  ; nmt_state :: Stored NMTState
  ; heartbeat_time :: Stored Uint16
  }
|]

newtype NMTState = NMTState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

nmtInitialising, nmtResetting, nmtResettingComm,
  nmtPreOperational, nmtOperational, nmtStopped :: NMTState
[nmtInitialising, nmtResetting, nmtResettingComm,
  nmtPreOperational, nmtOperational, nmtStopped]
    = map (NMTState . fromInteger) [0..5]

canopenTypes :: Module
canopenTypes = package "canopen_types" $ do
  defStruct (Proxy :: Proxy "device_info")


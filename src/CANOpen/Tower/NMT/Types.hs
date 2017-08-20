{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CANOpen.Tower.NMT.Types where

import Ivory.Language

newtype NMTState = NMTState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

nmtInitialising, nmtResetting, nmtResettingComm,
  nmtPreOperational, nmtOperational, nmtStopped :: NMTState
[nmtInitialising, nmtResetting, nmtResettingComm,
  nmtPreOperational, nmtOperational, nmtStopped]
    = map (NMTState . fromInteger) [0..5]

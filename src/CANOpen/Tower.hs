{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CANOpen.Tower where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import CANOpen.Ivory.Types
import CANOpen.Tower.LSS
import CANOpen.Tower.NMT
import Ivory.Serialize

getStdCANId :: (IvoryExpr (ref s ('Struct "can_message")),
                IvoryExpr (ref s ('Stored CANArbitrationField)), IvoryRef ref)
            => ref s ('Struct "can_message")
            -> Ivory eff Uint32
getStdCANId msg = do
  canarbit <- msg ~>* can_message_id
  cid <- assign $ toRep $ canarbit #. can_arbitration_id
  return (cid `iShiftR` 18)

canopenTower ::
            ChanOutput ('Struct "can_message")
         -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> Tower e (
                ChanInput ('Struct "device_info")
              , ChanOutput ('Struct "device_info")
            )
canopenTower res req = do
  canopenTowerDeps

  (di_in, di_out) <- channel
  (di_upstream_in, di_upstream_out) <- channel
  (lss_in, lss_out) <- channel
  (nmt_in, nmt_out) <- channel

  (lss_di_in, lss_di_out) <- lssTower lss_out req
  (nmt_di_in, nmt_di_out) <- nmtTower nmt_out req

  monitor "canopen_controller" $ do
    received <- stateInit "canopen_received" (ival (0 :: Uint32))
    lastmsg <- state "canopen_lastmsg"
    devinfo <- state "canopen_device_info"
    initDevinfo <- state "canopen_device_info_initial"

    stateLSS <- state "canopen_state_lss"

    dbg <- state "dbg"

    handler res "canmsg" $ do
      lsse <- emitter lss_in 1
      nmte <- emitter nmt_in 1
      callback $ \msg -> do
        received += 1
        refCopy lastmsg msg

        nid <- devinfo ~>* node_id
        cid <- getStdCANId msg

        store dbg (cid)

        isLSS <- deref stateLSS

        isNMTmsg <- assign $ cid ==? 0x0
        isLSSmsg <- assign $ cid ==? 0x7E5

        when (isLSS .&& isLSSmsg) $ do
          emit lsse msg

        -- forward NMT messages only if node_id is configured
        when (nid /=?0 .&& isNMTmsg) $ do
          emit nmte msg

    -- device info updates from lss
    handler lss_di_out "canopen_lss_devinfo" $ do
      nmte <- emitter nmt_di_in 1
      devie <- emitter di_in 1
      callback $ \di -> do
        refCopy devinfo di
        store stateLSS false
        -- update nmt node
        store (devinfo ~> nmt_state) nmtPreOperational
        emit nmte $ constRef devinfo
        emit devie $ constRef devinfo

    -- device info updates from nmt
    handler nmt_di_out "canopen_nmt_devinfo" $ do
      lsse <- emitter lss_di_in 1
      nmte <- emitter nmt_di_in 1
      devie <- emitter di_in 1
      callback $ \di -> do
        refCopy devinfo di

        nstate <- devinfo ~>* nmt_state
        when (nstate ==? nmtResetting .|| nstate ==? nmtResettingComm) $ do
          -- canopen node reset, switch back to initial state, enable lss
          refCopy devinfo initDevinfo
          store stateLSS true
          emit lsse $ constRef devinfo
          emit nmte $ constRef devinfo

        -- notify app
        emit devie $ constRef devinfo

    -- device info updates from upstream (initialization)
    handler di_upstream_out "canopen_devinfo" $ do
      lsse <- emitter lss_di_in 1
      callback $ \di -> do
        refCopy devinfo di
        refCopy initDevinfo di
        nid <- devinfo ~>* node_id
        hasLSS <- devinfo ~>* lss_supported
        assert (nid /=? 0 .|| hasLSS)
        when (nid ==? 0 .&& hasLSS) $ do
          emit lsse $ constRef devinfo
          store stateLSS true

  return (di_upstream_in, di_out)

canopenTowerDeps :: Tower e ()
canopenTowerDeps = do
  towerDepends canopenTypes
  towerModule canopenTypes

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

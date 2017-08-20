{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CANOpen.Tower
  ( canopenTower
  , CANOpenLEDs(..)
  ) where

import Ivory.Language
import Ivory.Language.Struct
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import CANOpen.Ivory.Types
import CANOpen.Tower.LSS
import CANOpen.Tower.NMT
import CANOpen.Tower.NMT.Types
import CANOpen.Tower.LED
import CANOpen.Tower.SDO
import CANOpen.Tower.PDO
import CANOpen.Tower.Types
import CANOpen.Tower.Utils
import Ivory.Serialize

import CANOpen.Tower.Attr
import CANOpen.Tower.Interface.Base.Dict

canopenTower :: ChanOutput ('Struct "can_message")
             -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
             -> CANOpenLEDs
             -> ObjDict
             -> Tower e ()
canopenTower res req leds objdictApp = do
  canopenTowerDeps

  (nid_update_in, nid_update_out) <- channel

  (lss_in, lss_out) <- channel
  (nmt_in, nmt_out) <- channel
  (sdo_in, sdo_out) <- channel
  (pdo_in, pdo_out) <- channel

  ledState <- ledStatusTower leds

  attrs@BaseAttrs{..} <- towerBaseAttrs initBaseAttrs
  objdictInternal <- objDictTower attrs

  objdictMerged <- objDictRouter objdictInternal objdictApp

  (lss_nid_in, lss_nid_out) <- lssTower lss_out req attrs
  (nmt_state_in, nmt_state_out) <- nmtTower nmt_out req nid_update_out attrs

  sdoTower sdo_out req objdictMerged nid_update_out
  pdoTower pdo_out req objdictMerged attrs

  monitor "canopen_controller" $ do
    received <- stateInit "canopen_received" (ival (0 :: Uint32))
    lastmsg <- state "canopen_lastmsg"

    stateLSS <- state "canopen_state_lss"
    stateNMT <- state "canopen_state_nmt"

    nodeId <- stateInit "canopen_nodeid" (ival (0 :: Uint8))

    dbg <- state "dbg"

    handler res "canmsg" $ do
      lsse <- emitter lss_in 1
      nmte <- emitter nmt_in 1
      sdoe <- emitter sdo_in 1
      pdoe <- emitter pdo_in 1
      callback $ \msg -> do
        received += 1
        refCopy lastmsg msg

        nid <- deref nodeId
        cid <- getStdCANId msg

        store dbg (cid)

        isLSS <- deref stateLSS

        isNMTmsg <- assign $ cid ==? 0x0
        isLSSmsg <- assign $ cid ==? 0x7E5
        isSDOmsg <- assign $ cid .& (safeCast sdoRequestBase) ==? (safeCast sdoRequestBase)
        isPDOmsg <- assign $ cid .& (safeCast pdoBase) ==? (safeCast pdoBase)

        when (isLSS .&& isLSSmsg) $ do
          emit lsse msg

        -- forward NMT messages only if node_id is configured
        when (nid /=? 0 .&& isNMTmsg) $ do
          emit nmte msg

        when (nid /=? 0 .&& isSDOmsg) $ do
          emit sdoe msg

        when (nid /=? 0 .&& isPDOmsg) $ do
          emit pdoe msg

    -- node id updates from lss
    handler lss_nid_out "canopen_lss_node_id" $ do
      nidE <- emitter nid_update_in 1
      ledStateE <- emitter ledState 2
      callbackV $ \nid -> do
        store nodeId nid
        store stateLSS false
        -- update nmt node
        -- XXX: store (devinfo ~> nmt_state) nmtPreOperational
        emitV nidE nid

        emitV ledStateE ledstateOk
        emitV ledStateE ledstatePreOperational

    -- updates from nmt
    handler nmt_state_out "canopen_nmt_state" $ do
      nmtE <- emitter (nmt_state_in) 1
      odIniE <- emitter (objdict_init objdictMerged) 1
      ledStateE <- emitter ledState 1

      callback $ \state -> do
        refCopy stateNMT state

        nstate <- deref stateNMT
        cond_ [
            nstate ==? nmtInitialising ==> do
              emitV nmtE nmtPreOperational
          , nstate ==? nmtResetting .|| nstate ==? nmtResettingComm ==> do
              -- canopen node reset, reset dictionary state
              -- transition to preOperational here
              emitV odIniE true
              emitV nmtE nmtPreOperational
              emitV ledStateE ledstateLSS
          , nstate ==? nmtStopped ==> do
              emitV ledStateE ledstateStopped
          , nstate ==? nmtPreOperational ==> do
              emitV ledStateE ledstatePreOperational
          , nstate ==? nmtOperational ==> do
              emitV ledStateE ledstateOperational
          ]

    handler systemInit "canopen_init" $ do
      ledStateE <- emitter ledState 1
      odIniE <- emitter (objdict_init objdictMerged) 1
      callback $ const $ do
        store stateLSS true
        emitV ledStateE ledstateLSS
        emitV odIniE true


canopenTowerDeps :: Tower e ()
canopenTowerDeps = do
  towerDepends dictTypes
  towerModule dictTypes

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

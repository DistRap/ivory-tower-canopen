{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CANOpen.Tower.NMT where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.CAN.Fragment

import Ivory.Serialize.LittleEndian

import CANOpen.Tower.Attr
import CANOpen.Tower.Interface.Base.Dict
import CANOpen.Tower.NMT.Types
import CANOpen.Tower.Utils

data NMTCMD
  = Start
  | Stop
  | PreOperation
  | ResetNode
  | ResetComm
  | HeartBeat

nmtCmd :: NMTCMD -> Uint8
nmtCmd Start = 0x01
nmtCmd Stop = 0x02
nmtCmd PreOperation = 0x80
nmtCmd ResetNode = 0x81
nmtCmd ResetComm = 0x82

heartBeatOffset :: Uint16
heartBeatOffset = 0x700

-- receives NMT (Network Management) messages from canopenTower
--
-- takes (required) node_id
-- outputs NMTState iff state changes
--
-- NMT message format
-- ID0 byte 1 node ID
--     byte 0 command
--
-- example NMT messages:
--
-- # start node 01
-- cansend can0 000#0101
--
-- # reset node 01
-- cansend can0 000#8101
nmtTower :: ChanOutput ('Struct "can_message")
         -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> ChanOutput ('Stored Uint8)
         -> BaseAttrs Attr
         -> Tower e (  ChanInput ('Stored NMTState)
                     , ChanOutput ('Stored NMTState))
nmtTower res req nid_update attrs = do
  (nmt_notify_in, nmt_notify_out) <- channel
  (nmt_set_in, nmt_set_out) <- channel

  p <- period (Milliseconds 1)

  monitor "nmt_controller" $ do
    received <- stateInit "nmt_received" (ival (0 :: Uint32))
    lastmsg <- state "nmt_lastmsg"
    lastcmd <- state "nmt_lastcmd"

    nmtState <- stateInit "nmt_state" (ival nmtInitialising)
    previous_nmtState <- stateInit "nmt_state_prev" (ival nmtInitialising)

    nodeId <- stateInit "heartbeat_node_id" (ival (0 :: Uint8))

    heartbeat_cnt <- stateInit "heartbeat_cnt" (ival (0 :: Uint16))
    heartbeat_enabled <- state "heartbeat_enabled"
    heartbeat_time <- state "heartbeat_time"

    attrHandler (producerHeartbeatTime attrs) $ do
      callbackV $ \time -> do
        store heartbeat_time time
        when (time >? 0) $ store heartbeat_enabled true

    handler systemInit "nmtinit" $ do
      nmtE <- emitter nmt_notify_in 1
      callback $ const $ emit nmtE (constRef nmtState)

    handler res "nmtcanmsg" $ do
      nmtE <- emitter nmt_notify_in 1
      reqE <- emitter (abortableTransmit req) 1
      callback $ \msg -> do
        received += 1
        refCopy lastmsg msg

        nid <- deref nodeId
        assert $ nid /=? 0
        mlen <- deref (msg ~> can_message_len)
        assert $ mlen ==? 2

        cmd <- deref (msg ~> can_message_buf ! 0)
        targetId <- deref (msg ~> can_message_buf ! 1)

        store lastcmd cmd
        let isCmd x = (cmd ==? nmtCmd x)

        when (targetId ==? 0 .|| targetId ==? nid) $ do
          cond_
            [ isCmd Start ==> do
                store nmtState nmtOperational
            , isCmd Stop ==> do
                store nmtState nmtStopped
            , isCmd PreOperation ==> do
                store nmtState nmtPreOperational

                -- emit bootmsg when entering pre-Operational
                empty <- local $ ival 0
                bootmsg <- canMsgUint8 (heartBeatOffset + safeCast nid) false $ constRef empty
                emit reqE bootmsg

                store heartbeat_enabled true

            , isCmd ResetNode ==> do
                store nmtState nmtResetting
                store heartbeat_enabled false
            , isCmd ResetComm ==> do
                store nmtState nmtResettingComm
                store heartbeat_enabled false
            ]

          cstate <- deref nmtState
          prevstate <- deref previous_nmtState
          when (cstate /=? prevstate) $ do
            store previous_nmtState cstate
            emit nmtE $ constRef nmtState

    handler nid_update "nmt_node_id" $ do
      callback $ refCopy nodeId

    handler nmt_set_out "nmt_set" $ do
      nmtE <- emitter nmt_notify_in 1
      callback $ \x -> refCopy nmtState x >> emit nmtE x

    handler p "per_heartbeat" $ do
      reqe   <- emitter (abortableTransmit req) 1
      callback $ const $ do
        cnt <- deref heartbeat_cnt
        ena <- deref heartbeat_enabled
        tim <- deref heartbeat_time

        heartbeat_cnt += 1

        when (ena .&& tim /=? 0) $ do
          when (cnt >=? tim) $ do
            nid <- deref nodeId
            state <- deref nmtState

            heartbeat_state <- local $ ival (0 :: Uint8)
            cond_
              [ state ==? nmtStopped ==> store heartbeat_state 4
              , state ==? nmtOperational ==> store heartbeat_state 5
              , state ==? nmtPreOperational ==> store heartbeat_state 127
              ]

            bootmsg <- canMsgUint8 (heartBeatOffset + safeCast nid) false $ constRef heartbeat_state
            emit reqe bootmsg

            store heartbeat_cnt 0

  return (nmt_set_in, nmt_notify_out)

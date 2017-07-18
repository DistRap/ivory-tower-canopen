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

import CANOpen.Ivory.Types
import CANOpen.Tower.Utils
import Ivory.Serialize.LittleEndian

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
-- takes (required) node_id from incoming device_info
-- outputs device_info iff state changes
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
         -> Tower e (
                ChanInput ('Struct "device_info")
              , ChanOutput ('Struct "device_info"))
nmtTower res req = do
  (di_upstream_in, di_upstream_out) <- channel
  (di_in, di_out) <- channel

  p <- period (Milliseconds 1)

  monitor "nmt_controller" $ do
    received <- stateInit "nmt_received" (ival (0 :: Uint32))
    lastmsg <- state "nmt_lastmsg"
    lastcmd <- state "nmt_lastcmd"
    devinfo <- state "nmt_device_info"

    nmtstate <- stateInit "nmt_state" (ival nmtInitialising)

    heartbeat_cnt <- stateInit "heartbeat_cnt" (ival (0 :: Uint16))
    heartbeat_enabled <- state "heartbeat_enabled"

    handler res "nmtcanmsg" $ do
      nmte <- emitter di_in 1
      reqe   <- emitter (abortableTransmit req) 1
      callback $ \msg -> do
        received += 1
        refCopy lastmsg msg

        nid <- devinfo ~>* node_id
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
                store nmtstate nmtOperational

            , isCmd Stop ==> do
                store nmtstate nmtStopped
            , isCmd PreOperation ==> do
                store nmtstate nmtPreOperational

                -- emit bootmsg when entering pre-Operational
                empty <- local $ ival 0
                bootmsg <- canMsgUint8 (heartBeatOffset + safeCast nid) false $ constRef empty
                emit reqe bootmsg

                store heartbeat_enabled true

            , isCmd ResetNode ==> do
                store nmtstate nmtResetting
                store heartbeat_enabled false
            , isCmd ResetComm ==> do
                store nmtstate nmtResettingComm
                store heartbeat_enabled false
            ]

          cstate <- deref nmtstate
          prevstate <- devinfo ~>* nmt_state
          when (cstate /=? prevstate) $ do
            store (devinfo ~> nmt_state) cstate
            emit nmte $ constRef devinfo

    handler di_upstream_out "nmt_devinfo" $ do
      callback $ refCopy devinfo

    handler p "per_heartbeat" $ do
      reqe   <- emitter (abortableTransmit req) 1
      callback $ const $ do
        cnt <- deref heartbeat_cnt
        ena <- deref heartbeat_enabled
        tim <- (devinfo ~>* heartbeat_time)

        heartbeat_cnt += 1

        when (ena .&& tim /=? 0) $ do
          when (cnt >=? tim) $ do
            nid <- devinfo ~>* node_id
            state <- deref nmtstate

            heartbeat_state <- local $ ival (0 :: Uint8)
            cond_
              [ state ==? nmtStopped ==> store heartbeat_state 4
              , state ==? nmtOperational ==> store heartbeat_state 5
              , state ==? nmtPreOperational ==> store heartbeat_state 127
              ]

            bootmsg <- canMsgUint8 (heartBeatOffset + safeCast nid) false $ constRef heartbeat_state
            emit reqe bootmsg

            store heartbeat_cnt 0

  return (di_upstream_in, di_out)

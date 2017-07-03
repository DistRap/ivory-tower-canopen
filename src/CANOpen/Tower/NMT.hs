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

nmtTower :: ChanOutput ('Struct "can_message")
         -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> Tower e (
                ChanInput ('Struct "device_info")
              , ChanOutput ('Struct "device_info"))
nmtTower res req = do
  (di_upstream_in, di_upstream_out) <- channel
  (di_in, di_out) <- channel

  monitor "nmt_controller" $ do
    received <- stateInit "nmt_received" (ival (0 :: Uint32))
    lastmsg <- state "nmt_lastmsg"
    lastcmd <- state "nmt_lastcmd"
    devinfo <- state "nmt_device_info"

    nmtstate <- stateInit "nmt_state" (ival nmtInitialising)

    handler res "nmtcanmsg" $ do
      nmte <- emitter di_in 1
      reqe   <- emitter (abortableTransmit req) 1
      callback $ \msg -> do
        received += 1
        refCopy lastmsg msg

        cmd <- deref (msg ~> can_message_buf ! 0)
        store lastcmd cmd
        let isCmd x = (cmd ==? nmtCmd x)

        cond_
          [ isCmd Start ==> do
              store nmtstate nmtOperational

              nid <- devinfo ~>* node_id
              empty <- local $ ival 0
              bootmsg <- canMsgUint8 (heartBeatOffset + safeCast nid) false $ constRef empty
              emit reqe bootmsg

          , isCmd Stop ==> do
              store nmtstate nmtStopped
          , isCmd PreOperation ==> do
              store nmtstate nmtPreOperational
          , isCmd ResetNode ==> do
              store nmtstate nmtResetting
          , isCmd ResetComm ==> do
              store nmtstate nmtResettingComm
          ]

        cstate <- deref nmtstate
        prevstate <- devinfo ~>* nmt_state
        when (cstate /=? prevstate) $ do
          store (devinfo ~> nmt_state) cstate
          emit nmte $ constRef devinfo

    handler di_upstream_out "nmt_devinfo" $ do
      callback $ refCopy devinfo

  return (di_upstream_in, di_out)

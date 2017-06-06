{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CANOpen.Tower.LSS where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.CAN.Fragment

import CANOpen.Ivory.Types
import CANOpen.Tower.Utils
import Ivory.Serialize.LittleEndian

lssTXCobID :: Uint16
lssTXCobID = 0x7E5
lssRXCobID :: Uint16
lssRXCobID = 0x7E4

data LSSCMD
  = SwitchModeGlobal
  | SwitchModeSelectiveBase
  | SwitchModeSelectiveVendor
  | SwitchModeSelectiveProduct
  | SwitchModeSelectiveRevision
  | SwitchModeSelectiveSerial
  | SwitchModeSelectiveReply
  | ConfigNodeID
  | ConfigBitTiming
  | ActivateBitTiming
  | StoreConfig
  | InquireVendor
  | InquireProduct
  | InquireRevision
  | InquireSerial
  | InquireNodeID
  | IdentifyRemoteSlavesVendor
  | IdentifyRemoteSlavesProduct
  | IdentifyRemoteSlavesRevisionLow
  | IdentifyRemoteSlavesRevisionHi
  | IdentifyRemoteSlavesSerialLow
  | IdentifyRemoteSlavesSerialHi
  | IdentifyRemoteSlavesReply
  | IdentifyNonConfigured
  | IdentifyNonConfiguredReply


lssCmd :: LSSCMD -> Uint8
lssCmd SwitchModeGlobal = 0x04
lssCmd SwitchModeSelectiveVendor = 0x40
lssCmd SwitchModeSelectiveProduct = 0x41
lssCmd SwitchModeSelectiveRevision = 0x42
lssCmd SwitchModeSelectiveSerial = 0x43
lssCmd SwitchModeSelectiveReply = 0x44
lssCmd ConfigNodeID = 0x11
lssCmd ConfigBitTiming = 0x13
lssCmd ActivateBitTiming = 0x13
lssCmd StoreConfig = 0x17
lssCmd InquireVendor = 0x5A
lssCmd InquireProduct = 0x5B
lssCmd InquireRevision = 0x5C
lssCmd InquireSerial = 0x5D
lssCmd InquireNodeID = 0x5E
lssCmd IdentifyRemoteSlavesVendor = 0x46
lssCmd IdentifyRemoteSlavesProduct = 0x47
lssCmd IdentifyRemoteSlavesRevisionLow = 0x48
lssCmd IdentifyRemoteSlavesRevisionHi = 0x49
lssCmd IdentifyRemoteSlavesSerialLow = 0x4a
lssCmd IdentifyRemoteSlavesSerialHi = 0x4b
lssCmd IdentifyRemoteSlavesReply = 0x4f
lssCmd IdentifyNonConfigured = 0x4C
lssCmd IdentifyNonConfiguredReply = 0x50


-- Layer Setting Services (CiA 305)
lssTower :: ChanOutput ('Struct "can_message")
         -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> Tower e (
                ChanInput ('Struct "device_info")
              , ChanOutput ('Struct "device_info")
            )
lssTower res req = do
  (di_in, di_out) <- channel
  (di_upstream_in, di_upstream_out) <- channel

  monitor "lss_controller" $ do
    received <- stateInit "lss_received" (ival (0 :: Uint32))
    lastmsg <- state "lss_lastmsg"
    devinfo <- state "lss_device_info"

    stateConfig <- stateInit "lss_state_config" (ival false)
    stateConfigured <- stateInit "lss_state_configured" (ival false)
    lastcmd <- state "lss_lastcmd"

    selective <- state "lss_selective" -- switch mode selective in progress
    identify <- state "lss_identify" -- identify in progress

    handler res "lsscanmsg" $ do
      devie <- emitter di_in 1
      reqe   <- emitter (abortableTransmit req) 1
      callback $ \msg -> do
        received += 1
        refCopy lastmsg msg

        cmd <- deref (msg ~> can_message_buf ! 0)
        store lastcmd cmd
        let isCmd x = (cmd ==? lssCmd x)

        isConfig <- deref stateConfig
        isConfigured <- deref stateConfigured
        isSelective <- deref selective
        isIdentify <- deref identify

        cond_
          [ isCmd SwitchModeGlobal ==> do
              store stateConfig true

          , isCmd SwitchModeSelectiveVendor ==> do
              vendor <- lssGetU32 SwitchModeSelectiveVendor (msg ~> can_message_buf)
              ourVendor <- devinfo ~>* vendor_id
              store selective (vendor ==? ourVendor)

          , isCmd SwitchModeSelectiveProduct .&& isSelective ==> do
              code <- lssGetU32 SwitchModeSelectiveProduct (msg ~> can_message_buf)
              ourCode <- devinfo ~>* product_code
              store selective (code ==? ourCode)

          , isCmd SwitchModeSelectiveRevision .&& isSelective ==> do
              rev <- lssGetU32 SwitchModeSelectiveRevision (msg ~> can_message_buf)
              ourRev <- devinfo ~>* revision_number
              store selective (rev ==? ourRev)

           , isCmd SwitchModeSelectiveSerial .&& isSelective ==> do
              ser <- lssGetU32 SwitchModeSelectiveSerial (msg ~> can_message_buf)
              ourSer <- devinfo ~>* serial_number
              when (ser ==? ourSer) $ do
                store stateConfig true

                reply <- lssMsg SwitchModeSelectiveReply
                emit reqe reply

              store selective false

          , isCmd ConfigNodeID .&& isConfig ==> do
              let err_code :: Uint8
                  err_code = 0
                  specific_err_code :: Uint8
                  specific_err_code = 0
              nid <- deref (msg ~> can_message_buf ! 1)
              store (devinfo ~> node_id) nid

              ec <- local $ ival (safeCast err_code `iShiftL` 8 + safeCast specific_err_code) 
              reply <- lssMsgU16 ConfigNodeID $ constRef ec
              emit reqe reply

          , isCmd StoreConfig .&& isConfig ==> do
              store stateConfigured true
              store stateConfig false

              let err_code :: Uint8
                  err_code = 0
                  specific_err_code :: Uint8
                  specific_err_code = 0

              ec <- local $ ival (safeCast err_code `iShiftL` 8 + safeCast specific_err_code) 
              reply <- lssMsgU16 StoreConfig $ constRef ec
              emit reqe reply

              -- we do exit LSS here sending device info to parent canOpen tower
              -- this might not be according to spec but we don't have non-volatile
              -- id storage for now
              emit devie $ constRef devinfo

          , isCmd InquireVendor ==> do
              reply <- lssMsgU32 InquireVendor $ constRef (devinfo ~> vendor_id)
              emit reqe reply
          , isCmd InquireProduct ==> do
              reply <- lssMsgU32 InquireProduct $ constRef (devinfo ~> product_code)
              emit reqe reply
          , isCmd InquireRevision ==> do
              reply <- lssMsgU32 InquireRevision $ constRef (devinfo ~> revision_number)
              emit reqe reply
          , isCmd InquireSerial ==> do
              reply <- lssMsgU32 InquireSerial $ constRef (devinfo ~> serial_number)
              emit reqe reply
          , isCmd InquireNodeID ==> do
              reply <- lssMsgU8 InquireNodeID $ constRef (devinfo ~> node_id)
              emit reqe reply
          , isCmd IdentifyNonConfigured ==> do
              unless isConfigured $ do
                reply <- lssMsg IdentifyNonConfiguredReply
                emit reqe reply

          , isCmd IdentifyRemoteSlavesVendor ==> do
              vendor <- lssGetU32 IdentifyRemoteSlavesVendor (msg ~> can_message_buf)
              ourVendor <- devinfo ~>* vendor_id
              store identify (vendor ==? ourVendor)

          , isCmd IdentifyRemoteSlavesProduct .&& isIdentify ==> do
              code <- lssGetU32 IdentifyRemoteSlavesProduct (msg ~> can_message_buf)
              ourCode <- devinfo ~>* product_code
              store identify (code ==? ourCode)

          , isCmd IdentifyRemoteSlavesRevisionLow .&& isIdentify ==> do
              rev <- lssGetU32 IdentifyRemoteSlavesRevisionLow (msg ~> can_message_buf)
              ourRev <- devinfo ~>* revision_number
              store identify (rev <=? ourRev)

          , isCmd IdentifyRemoteSlavesRevisionHi .&& isIdentify ==> do
              rev <- lssGetU32 IdentifyRemoteSlavesRevisionHi (msg ~> can_message_buf)
              ourRev <- devinfo ~>* revision_number
              store identify (rev >=? ourRev)

          , isCmd IdentifyRemoteSlavesSerialLow .&& isIdentify ==> do
              ser <- lssGetU32 IdentifyRemoteSlavesSerialLow (msg ~> can_message_buf)
              ourSer <- devinfo ~>* serial_number
              store identify (ser <=? ourSer)

          , isCmd IdentifyRemoteSlavesSerialHi .&& isIdentify ==> do
              ser <- lssGetU32 IdentifyRemoteSlavesSerialHi (msg ~> can_message_buf)
              ourSer <- devinfo ~>* serial_number
              when (ser >=? ourSer) $ do

                reply <- lssMsg IdentifyRemoteSlavesReply
                emit reqe reply

              store identify false
          ]


    handler di_upstream_out "lss_devinfo" $ do
      callback $ refCopy devinfo

  return (di_upstream_in, di_out)

-- unpack Uint32 from incoming LSS CAN message
lssGetU32 :: LSSCMD
          -> ConstRef s1 ('Array 8 ('Stored Uint8))
          -> Ivory ('Effects r b ('Scope s2)) (Uint32)
lssGetU32 cmd arr = do
  inCmd <- deref (arr ! 0)
  assert $ (lssCmd cmd) ==? inCmd
  val <- local $ ival (0 :: Uint32)
  unpackFrom arr 1 val
  v <- deref val
  return v

-- pack Uint32 into lss frame [cmd,u4,u3,u2,u1]
lssMsgU32 :: LSSCMD
          -> ConstRef s2 ('Stored Uint32)
          -> Ivory
               ('Effects r b ('Scope s1))
               (ConstRef ('Stack s1) ('Struct "can_message"))
lssMsgU32 cmd x = do
  arr <- local $ izerolen (Proxy :: Proxy 5)
  store (arr ! 0) (lssCmd cmd)
  packInto arr 1 x
  canMsg lssRXCobID false (constRef arr) 5

-- pack Uint16 into lss frame [cmd,u2,u1]
lssMsgU16 :: LSSCMD
          -> ConstRef s2 ('Stored Uint16)
          -> Ivory
               ('Effects r b ('Scope s1))
               (ConstRef ('Stack s1) ('Struct "can_message"))
lssMsgU16 cmd x = do
  arr <- local $ izerolen (Proxy :: Proxy 3)
  store (arr ! 0) (lssCmd cmd)
  packInto arr 1 x
  canMsg lssRXCobID false (constRef arr) 3

-- pack Uint8 into lss frame [cmd,u]
lssMsgU8 :: LSSCMD
         -> ConstRef s2 ('Stored Uint8)
         -> Ivory
              ('Effects r b ('Scope s1))
              (ConstRef ('Stack s1) ('Struct "can_message"))
lssMsgU8 cmd x = do
  arr <- local $ izerolen (Proxy :: Proxy 2)
  store (arr ! 0) (lssCmd cmd)
  packInto arr 1 x
  canMsg lssRXCobID false (constRef arr) 2

lssMsg :: LSSCMD
       -> Ivory
            ('Effects r b ('Scope s1))
            (ConstRef ('Stack s1) ('Struct "can_message"))
lssMsg cmd = do
  arr <- local $ izerolen (Proxy :: Proxy 1)
  store (arr ! 0) (lssCmd cmd)
  canMsg lssRXCobID false (constRef arr) 1

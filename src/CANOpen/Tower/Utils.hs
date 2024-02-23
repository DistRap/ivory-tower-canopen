{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CANOpen.Tower.Utils where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Serialize.LittleEndian
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import CANOpen.Tower.Types
import GHC.TypeLits (KnownNat)

canMsg
  :: ( GetAlloc eff ~ 'Scope s1
     , KnownConstancy c
     , GHC.TypeLits.KnownNat m
     )
  => Uint16
  -> IBool
  -> Pointer 'Valid c s2 ('Array m ('Stored Uint8))
  -> Ix 9
  -> Ivory eff (ConstRef ('Stack s1) ('Struct "can_message"))
canMsg uid rtr arr len = do
  let msgid = standardCANID (fromRep uid) (boolToBit rtr)

  reply <- local $ istruct
    [ can_message_id .= ival msgid
    , can_message_len .= ival len
    ]
  arrayCopy (reply ~> can_message_buf) arr 0 (fromIx len)
  -- won't type check - can't do refcopy cause we don't know len
  --refCopy (reply ~> can_message_buf) arr
  return $ constRef reply

canMsgUint64
  :: Uint16
  -> IBool
  -> ConstRef s2 ('Stored Uint64)
  -> Ivory
       ('Effects r b ('Scope s1))
       (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint64 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 8)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 8

canMsgUint32
  :: Uint16
  -> IBool
  -> ConstRef s2 ('Stored Uint32)
  -> Ivory
       ('Effects r b ('Scope s1))
       (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint32 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 4)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 4

canMsgUint16
  :: Uint16
  -> IBool
  -> ConstRef s2 ('Stored Uint16)
  -> Ivory
       ('Effects r b ('Scope s1))
       (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint16 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 2)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 2

canMsgUint8
  :: Uint16
  -> IBool
  -> ConstRef s2 ('Stored Uint8)
  -> Ivory
       ('Effects r b ('Scope s1))
       (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint8 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 1)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 1

getStdCANId
  :: KnownConstancy c
  => Pointer 'Valid c s ('Struct "can_message")
  -> Ivory eff Uint32
getStdCANId msg = do
  canarbit <- msg ~>* can_message_id
  cid <- assign $ toRep $ canarbit #. can_arbitration_id
  return (cid `iShiftR` 18)

-- route objDict requests to internal or application dictionary
--   if address is over 0x6000 requests go to application dict
--   init is shared
--   XXX: we should return seprate inits so we can reset
--   communication and application parameters separately
objDictRouter :: ObjDict -> ObjDict -> Tower e ObjDict
objDictRouter internal application = do
  (objdict_init', objdict_init_local) <- channel
  (objdict_get_in', objdict_get_in_local) <- channel
  (objdict_get_out_local, objdict_get_out') <- channel
  (objdict_set_in', objdict_set_in_local) <- channel
  (objdict_set_out_local, objdict_set_out') <- channel

  monitor "objdictRouter" $ do
    handler objdict_init_local "init" $ do
      int <- emitter (objdict_init internal) 1
      app <- emitter (objdict_init application) 1
      callback $ const $ do
        emitV int true
        emitV app true

    -- proxy get(s)
    handler objdict_get_in_local "get_in" $ do
      int <- emitter (objdict_get_in internal) 1
      app <- emitter (objdict_get_in application) 1
      callback $ \mux -> do
        address <- mux ~>* addr
        ifte_ (address >=? 0x6000) (emit app mux) (emit int mux)

    handler (objdict_get_out internal) "get_out_internal" $ do
      merged_get_out <- emitter objdict_get_out_local 1
      callback $ emit merged_get_out

    handler (objdict_get_out application) "get_out_app" $ do
      merged_get_out <- emitter objdict_get_out_local 1
      callback $ emit merged_get_out

    -- proxy set(s)
    handler objdict_set_in_local "set_in" $ do
      int <- emitter (objdict_set_in internal) 1
      app <- emitter (objdict_set_in application) 1
      callback $ \mp -> do
        address <- mp ~> mp_mux ~>* addr
        ifte_ (address >=? 0x6000) (emit app mp) (emit int mp)

    handler (objdict_set_out internal) "get_out_internal" $ do
      merged_set_out <- emitter objdict_set_out_local 1
      callback $ emit merged_set_out

    handler (objdict_set_out application) "get_out_app" $ do
      merged_set_out <- emitter objdict_set_out_local 1
      callback $ emit merged_set_out

  return $ ObjDict {
      objdict_init = objdict_init'
    , objdict_get_in = objdict_get_in'
    , objdict_get_out = objdict_get_out'
    , objdict_set_in = objdict_set_in'
    , objdict_set_out = objdict_set_out'
    }

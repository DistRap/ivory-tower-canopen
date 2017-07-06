{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CANOpen.Tower.Utils where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Serialize.LittleEndian
import Ivory.Tower.HAL.Bus.CAN
import GHC.TypeLits (KnownNat)

canMsg :: (GetAlloc eff ~ 'Scope s1,
           IvoryExpr (r s2 ('Stored Uint8)),
           IvoryExpr (r s2 ('Array m ('Stored Uint8))), IvoryRef r,
           GHC.TypeLits.KnownNat m)
       => Uint16
       -> IBool
       -> r s2 ('Array m ('Stored Uint8))
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

canMsgUint32 :: Uint16
             -> IBool
             -> ConstRef s2 ('Stored Uint32)
             -> Ivory
                  ('Effects r b ('Scope s1))
                  (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint32 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 4)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 4

canMsgUint16 :: Uint16
             -> IBool
             -> ConstRef s2 ('Stored Uint16)
             -> Ivory
                  ('Effects r b ('Scope s1))
                  (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint16 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 2)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 2

canMsgUint8 :: Uint16
            -> IBool
            -> ConstRef s2 ('Stored Uint8)
            -> Ivory
                 ('Effects r b ('Scope s1))
                 (ConstRef ('Stack s1) ('Struct "can_message"))
canMsgUint8 uid rtr x = do
  arr <- local $ izerolen (Proxy :: Proxy 1)
  packInto arr 0 x
  canMsg uid rtr (constRef arr) 1

getStdCANId :: (IvoryExpr (ref s ('Struct "can_message")),
                IvoryExpr (ref s ('Stored CANArbitrationField)), IvoryRef ref)
            => ref s ('Struct "can_message")
            -> Ivory eff Uint32
getStdCANId msg = do
  canarbit <- msg ~>* can_message_id
  cid <- assign $ toRep $ canarbit #. can_arbitration_id
  return (cid `iShiftR` 18)

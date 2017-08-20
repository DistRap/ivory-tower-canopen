{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleInstances #-}

module CANOpen.Tower.SDO where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.CAN.Fragment

import CANOpen.Ivory.Types
import CANOpen.Tower.Utils
import CANOpen.Tower.Types
import CANOpen.Tower.SDO.Types
import Ivory.Serialize.LittleEndian

sdoRequestBase :: Uint16
sdoRequestBase = 0x600
sdoReplyBase :: Uint16
sdoReplyBase = 0x580

[ivory|
 bitdata SDORequest :: Bits 8 = sdo_request
   { sdo_ccs         :: SDOClientCommandSpecifier
   , sdo_header      :: Bits 5
   }

 bitdata SDOReply :: Bits 8 = sdo_reply
   { sdo_reply_scs         :: SDOServerCommandSpecifier
   , sdo_reply_header      :: Bits 5
   }

 bitdata SDOInitHeader :: Bits 5 = sdo_init_header
   { _                  :: Bit
   , sdo_num_bytes      :: Bits 2
   , sdo_expedited      :: Bit -- transfer type (0 normal, 1 expedited)
   , sdo_size_indicator :: Bit -- size (0 data set size not indicated, 1 indicated)
   }


 bitdata SDOSegmentHeader :: Bits 5 = sdo_segment_header
   { sdo_toggle         :: Bit
   , sdo_seg_num_bytes  :: Bits 3
   , sdo_continued      :: Bit
   }
|]

uploadReply :: IBool -> IBool -> Uint8 -> SDOReply
uploadReply expedited size_indicator size =
  fromRep $ withBits 0 $ do
    setField sdo_reply_scs scs_upload_init
    setField sdo_reply_header header
  where
    header = fromRep $ withBits 0 $ do
      setField sdo_expedited $ boolToBit expedited
      setField sdo_size_indicator $ boolToBit size_indicator
      setField sdo_num_bytes $ fromRep size

uploadSegmentReply :: IBool -> Uint8 -> IBool -> SDOReply
uploadSegmentReply toggle size continued =
  fromRep $ withBits 0 $ do
    setField sdo_reply_scs scs_download_segment
    setField sdo_reply_header header
  where
    header = fromRep $ withBits 0 $ do
      setField sdo_toggle $ boolToBit toggle
      setField sdo_continued $ boolToBit continued
      setField sdo_seg_num_bytes $ fromRep size


downloadReply :: SDOReply
downloadReply =
  fromRep $ withBits 0 $ do
    setField sdo_reply_scs scs_download_init

abortReply :: SDOReply
abortReply =
  fromRep $ withBits 0 $ do
    setField sdo_reply_scs scs_abort

abort :: ConstRef s ('Struct "mux")
      -> SDOError
      -> Ivory ('Effects r b ('Scope s1))
               (ConstRef ('Stack s1) ('Array 8 ('Stored Uint8)))
abort mux errCode = do
   arr <- local $ izerolen (Proxy :: Proxy 8)
   err <- local $ ival (unSDOError errCode)
   store (arr ! 0) $ toRep $ abortReply
   -- pack multiplexer (3 bytes)
   packInto arr 1 mux
   packInto arr 4 (constRef err)
   return $ constRef arr

abortDictErr :: ConstRef s ('Struct "mux")
             -> DictError
             -> Ivory ('Effects r b ('Scope s1))
                      (ConstRef ('Stack s1) ('Array 8 ('Stored Uint8)))
abortDictErr mux dictErr = do
  sdoErr <- call dictErrToSDOErr dictErr
  abort mux sdoErr

sdoTower :: ChanOutput ('Struct "can_message")
         -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> ObjDict
         -> ChanOutput ('Stored Uint8)
         -> Tower e ()
sdoTower res req ObjDict{..} nid_update = do

  towerDepends sdoTypes
  towerModule sdoTypes

  monitor "sdo_controller" $ do
    received <- stateInit "sdo_received" (ival (0 :: Uint32))
    lastmsg <- state "sdo_lastmsg"
    nodeId <- state "sdo_node_id"

    upload <- stateInit "sdo_upload" $ ival false
    uploadMux <- state "sdo_upload_mux"

    download <- stateInit "sdo_download" $ ival false
    downloadMux <- state "sdo_download_mux"

    segmented_download <- stateInit "sdo_segmented_download" $ ival false
    segmented_download_len <- stateInit "sdo_segmented_len" $ ival (0 :: Uint16)

    getres_local <- state "getres_local"
    setres_local <- state "setres_local"

    handler res "sdomsg" $ do
      reqe   <- emitter (abortableTransmit req) 1
      objdictGetE <- emitter objdict_get_in 1
      objdictSetE <- emitter objdict_set_in 1
      callback $ \msg -> do
        nid <- deref nodeId
        cid <- getStdCANId msg

        when (safeCast nid + safeCast sdoRequestBase ==? cid) $ do
          received += 1
          refCopy lastmsg msg

          buf <- assign (msg ~> can_message_buf)

          cmd <- deref (buf ! 0)
          sdocmd <- assign $ fromRep cmd
          cond_
            [ sdocmd #. sdo_ccs ==? ccs_upload_init ==> do
                store upload true

                mux <- local (istruct [] :: Init ('Struct "mux"))
                unpackFrom buf 1 mux

                -- object dict lookup
                refCopy uploadMux (constRef mux)
                emit objdictGetE (constRef mux)

            , sdocmd #. sdo_ccs ==? ccs_download_init ==> do
                store download true

                mux <- local (istruct [] :: Init ('Struct "mux"))
                unpackFrom buf 1 mux

                muxpack <- local $ istruct []

                refCopy (muxpack ~> mp_mux) mux

                sdohdr <- assign $ fromRep $ toRep $ sdocmd #. sdo_header
                isExpedited <- assign $ bitToBool (sdohdr #. sdo_expedited)
                sizeIndicated <- assign $ bitToBool (sdohdr #. sdo_size_indicator)
                -- only valid if expedited and sizeIndicated
                -- if valid, contains number of bytes in data
                -- that do not contain data
                numBytes <- assign $ safeCast $ 4 - (toRep $ sdohdr #. sdo_num_bytes)

                ifte_ isExpedited
                  (do
                     ifte_ sizeIndicated
                       (do
                          arrayCopyFromOffset (muxpack ~> mp_buf ~> stringDataL) buf 4 numBytes
                          store (muxpack ~> mp_buf ~> stringLengthL) numBytes

                          -- object dict set
                          refCopy downloadMux (constRef mux)
                          emit objdictSetE (constRef muxpack)
                       )
                       (do
                          -- expedited download with no size indicated
                          -- what to do?
                          return ()
                       )
                  )
                  (do
                     -- segmented transfers
                     ifte_ sizeIndicated
                       (do
                          len <- local $ ival (0 :: Uint16)
                          unpackFrom buf 4 len
                          store segmented_download true
                          refCopy segmented_download_len len
                          return ()
                       )
                       (do
                          -- segmented download with no size indicated
                          -- reserved
                          return ()
                       )
                  )
            ]

    -- objdict get reply (craft sdo upload response)
    handler objdict_get_out "objdict_get_out" $ do
      reqe   <- emitter (abortableTransmit req) 1
      callback $ \getres -> do
        refCopy getres_local getres

        nid <- deref nodeId
        getOk <- getres ~>* getres_ok

        when getOk $ do
          buf <- assign $ getres ~> getres_buf

          len <- buf ~>* stringLengthL
          assert (len <=? 255)
          -- XXX: handle segmented upload when len is larger than 4
          ulen <- fmap (bitCast . (signCast :: Sint32 -> Uint32)) $ buf ~>* stringLengthL

          let expedited = true
              size_indicated = true

          arr <- local $ izerolen (Proxy :: Proxy 8)
          store (arr ! 0) $ toRep $ uploadReply expedited size_indicated ulen
          -- pack multiplexer (3 bytes)
          packInto arr 1 (getres ~> getres_mux)

          -- bits 4-8 may be data if expedited (e=1, s=1)
          -- or data length if (e=0, s=1)
          arrayCopy arr (buf ~> stringDataL) 4 len

          msg <- canMsg (sdoReplyBase + safeCast nid) false (constRef arr) 8
          emit reqe msg

        unless getOk $ do
          err <- getres ~>* getres_error
          arr <- abortDictErr (getres ~> getres_mux) err
          msg <- canMsg (sdoReplyBase + safeCast nid) false arr 8
          emit reqe msg

    -- objdict set reply (craft sdo download response)
    handler objdict_set_out "objdict_set_out" $ do

      reqe   <- emitter (abortableTransmit req) 1
      callback $ \setres -> do
        refCopy setres_local setres

        nid <- deref nodeId
        setOk <- setres ~>* setres_ok

        when setOk $ do
          arr <- local $ izerolen (Proxy :: Proxy 8)
          store (arr ! 0) $ toRep $ downloadReply
          -- pack multiplexer (3 bytes)
          packInto arr 1 (setres ~> setres_mux)

          msg <- canMsg (sdoReplyBase + safeCast nid) false (constRef arr) 8
          emit reqe msg

        unless setOk $ do
          err <- setres ~>* setres_error
          arr <- abortDictErr (setres ~> setres_mux) err
          msg <- canMsg (sdoReplyBase + safeCast nid) false arr 8
          emit reqe msg

    handler nid_update "sdo_node_id" $ do
      callback $ refCopy nodeId

canToUint64 :: (GetAlloc eff ~ 'Scope s1)
            => ConstRef s ('Array 8 ('Stored Uint8))
            -> Ivory eff (ConstRef ('Stack s1) ('Stored Uint64))
canToUint64 buf = do
  out <- local $ ival (0 :: Uint64)
  arrayMap $ \i -> do
    x <- fmap (safeCast :: Uint8 -> Uint64) $ deref (buf ! i)
    store out (x `iShiftL` (8 - (safeCast $ (signCast :: Sint32 -> Uint32) $ fromIx i)))

  return $ constRef out

arrayCopyFromOffset :: ( ANat n, ANat m, IvoryRef r
                       , IvoryExpr (r s2 ('Array m ('Stored t)))
                       , IvoryExpr (r s2 ('Stored t))
                       , IvoryStore t
                       )
                    => Ref s1 ('Array n ('Stored t))
                    -> r s2 ('Array m ('Stored t))
                    -> Sint32
                    -> Sint32
                    -> Ivory eff ()
arrayCopyFromOffset to from fromOffset end = do
  assert (fromOffset >=? 0 .&& fromOffset <? frLen)
  assert (end        >=? 0 .&& end       <=? toLen)
  arrayMap $ go
  where
  -- The index is w.r.t. the from array.
  go ix =
    cond_
      [   -- We've reached the @end@ index: stop copying.
          (fromIx ix >=? end)
      ==> return ()
          -- We've reached the end of the @to@ array: stop copying.
      ,   (fromIx ix >=? toLen)
      ==> return ()
      ,   true
      ==> (deref (from ! mkIx ix) >>= store (to ! ix))
      ]

  toLen = arrayLen to
  frLen = arrayLen from

  mkIx ix = toIx (fromOffset + fromIx ix)

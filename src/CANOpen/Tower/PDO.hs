{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module CANOpen.Tower.PDO where

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

import CANOpen.Ivory.Types.PdoCommParam
import CANOpen.Ivory.Types.PdoCommMap
--import CANOpen.Ivory.Types.VarArrayPdoCommMap

import CANOpen.Tower.Attr
import CANOpen.Tower.Interface.Base.Dict

pdoBase :: Uint16
pdoBase = 0x180

pdoTower :: ChanOutput ('Struct "can_message")
         -> AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> ObjDict
         -> BaseAttrs Attr
         -> Tower e ()
pdoTower res req ObjDict{..} BaseAttrs{..} = do
  monitor "pdo_controller" $ do
--    attrHandler (head $ tpdos attrs) $ callback $ const $ return ()
--    (\x -> attrHandler x $ callback $ const $ return ())
--     (head $ tpdos attrs)
--
    tstates <- mapM (\(p, m) -> attrState p) tpdos

    mapM_ (\((p, m), s) -> attrHandler p $ callback $ refCopy s)
     (zip tpdos tstates)


    received <- stateInit "pdo_received" (ival (0 :: Uint32))
    handled <- stateInit "pdo_handled" (ival (0 :: Uint32))
    --reqe   <- emitter (abortableTransmit req) 1

    handler res "pdomsg" $ do

      callback $ \msg -> do
        received += 1
        cid <- getStdCANId msg

        flip mapM_ tstates $ \s -> do
          cidPDO <- s ~>* cob_id
          when (cid ==? cidPDO) $ do
            handled += 1

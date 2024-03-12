{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.OS.Posix.Tower
import Ivory.OS.Posix.Tower.IO
import Ivory.OS.Posix.Tower.Serial

import C4D.SLCAN
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

import CANOpen.Tower
import CANOpen.Tower.Attr
import CANOpen.Tower.LED

import CANOpen.Ivory.Types
import CANOpen.Tower.Interface.Cia402.Dict

app :: Tower e ()
app = do
  uartTowerDeps
  mapM_ towerDepends typeModules
  mapM_ towerModule typeModules


  (toCanIn, toCanOut) <- channel
  (fromCanIn, fromCanOut) <- channel

  (buffered_ostream, istream) <- serialIO
  ostream <- uartUnbuffer (
    buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  slCANTowerSimple ostream istream toCanIn fromCanOut-- (const (return ()))

  attrs@Cia402Attrs{..} <- towerCia402Attrs initCia402Attrs
  objdict <- objDictTower attrs

  fromCanInWrapped <- abortableWrapper fromCanIn
  --(fromCanInWrapped, fromCanOutWrapped) <- channel

  canopenTower
    toCanOut
    fromCanInWrapped
    defaultCANOpenConfig
    emptyLEDs
    objdict

  p <- period (Milliseconds 100)

  monitor "fake_controller" $ do
    currVelocity <- state "currVelocity"
    toVelocity <- state "toVelocity"

    handler p "per" $ do
      vaE <- attrEmitter velocityActual

      callback $ const $ do
        cv <- deref currVelocity
        tv <- deref toVelocity
        cond_ [
            tv >? cv ==> currVelocity += 1
          , tv <? cv ==> currVelocity += (-1)
          ]

        emit vaE (constRef currVelocity)

    attrHandler targetVelocity $ callback $ refCopy toVelocity

main :: IO ()
main = compileTowerPosix (const $ return ()) app

abortableWrapper chan = do
  (abortableTransmit, request) <- channel
  (response, abortableComplete) <- channel
  (abortableAbort, abort) <- channel

  monitor "make_abortable" $ do
    handler request "to_abortable" $ do
      resp <- emitter response 1
      unwrapped <- emitter chan 1
      callback $ \canmsg -> do
        emit unwrapped canmsg
        emitV resp true

  return AbortableTransmit { .. }

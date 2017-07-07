{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module CANOpen.Tower.LED where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module


newtype LEDState = LEDState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

ledstateOk, ledstateWarn, ledstateLSS, ledstateErrorControlEvent,
 ledstateSyncError, ledstateBusOff,
 ledstateStopped, ledstatePreOperational, ledstateOperational :: LEDState

[ledstateOk, ledstateWarn, ledstateLSS, ledstateErrorControlEvent,
 ledstateSyncError, ledstateBusOff,
 ledstateStopped, ledstatePreOperational, ledstateOperational]
  = map (LEDState . fromInteger) [0..8]

data CANOpenLEDs =
  CANOpenLEDs
    { leds_init       :: forall eff . Ivory eff ()
    , leds_err_on     :: forall eff . Ivory eff ()
    , leds_err_off    :: forall eff . Ivory eff ()
    , leds_run_on     :: forall eff . Ivory eff ()
    , leds_run_off    :: forall eff . Ivory eff ()
    }

emptyLEDs :: CANOpenLEDs
emptyLEDs =
  CANOpenLEDs
    { leds_init       = return ()
    , leds_err_on     = return ()
    , leds_err_off    = return ()
    , leds_run_on     = return ()
    , leds_run_off    = return ()
    }

-- control error and run leds according to LEDState
ledStatusTower :: CANOpenLEDs -> Tower e (ChanInput ('Stored LEDState))
ledStatusTower CANOpenLEDs{..} = do
  (stIn, stOut) <- channel

  err_led <- ledController $ LED leds_init leds_err_on leds_err_off
  run_led <- ledController $ LED leds_init leds_run_on leds_run_off

  monitor "led_status_controller" $ do

    st <- state "led_controller_status"

    handler stOut "status_out" $ do

      errLed <- emitter err_led 1
      runLed <- emitter run_led 1

      callbackV $ \stat -> do
        store st stat

        cond_
          [
          -- err led
            stat ==? ledstateOk ==> emitV errLed ledOff
          , stat ==? ledstateWarn ==> emitV errLed ledFlashSingle
          , stat ==? ledstateErrorControlEvent ==> emitV errLed ledFlashDouble
          , stat ==? ledstateSyncError ==> emitV errLed ledFlashTriple
          , stat ==? ledstateBusOff ==> emitV errLed ledOn

          -- run led
          , stat ==? ledstateStopped ==> emitV runLed ledFlashSingle
          , stat ==? ledstatePreOperational ==> emitV runLed ledBlink
          , stat ==? ledstateOperational ==> emitV runLed ledOn

          -- both
          , stat ==? ledstateLSS ==> do
              emitV errLed ledFlicker
              emitV runLed ledFlicker
          ]

  return stIn

newtype LEDMode = LEDMode Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

ledOff, ledOn, ledFlicker, ledBlink, ledFlashSingle,
 ledFlashDouble, ledFlashTriple :: LEDMode
[ledOff, ledOn, ledFlicker, ledBlink, ledFlashSingle,
 ledFlashDouble, ledFlashTriple]
  = map (LEDMode . fromInteger) [0..6]

data LED =
  LED
    { led_init   :: forall eff . Ivory eff ()
    , led_on     :: forall eff . Ivory eff ()
    , led_off    :: forall eff . Ivory eff ()
    }

-- LED controller with CANOpen modes (CiA 303-3)
ledController :: LED -> Tower e (ChanInput ('Stored LEDMode))
ledController LED{..} = do
  p <- period (Milliseconds 50)
  (ledIn, ledOut) <- channel

  monitor "led_controller" $ do
    monitorModuleDef $ hw_moduledef
    handler systemInit "init" $ callback $ const $ led_init

    led_is_on <- state "led_on"
    led_state <- state "led_state"
    led_toggle <- state "led_toggle"
    led_cnt <- state "led_cnt"

    handler ledOut "modeOut" $ callbackV $ \stat -> do
      store led_cnt (0 :: Uint8)
      store led_is_on false
      store led_state stat
      led_off

    handler p "per" $ callback $ const $ do
      let countFlashes x = do
            cnt <- deref led_cnt
            when (cnt .% 4 ==? 0 .&& cnt <=? (4*x+4*(x-1))) $ store led_toggle true

            led_cnt += 1

            when (cnt ==? (4*x+4*(x-1)) + 1 + 20) $ do
              store led_cnt (0 :: Uint8)

      isOn <- deref led_is_on

      ledSt <- deref led_state

      store led_toggle false

      cond_
        [ ledSt ==? ledOff ==> led_off
        , ledSt ==? ledOn ==> led_on
        , ledSt ==? ledFlicker ==> store led_toggle true
        , ledSt ==? ledBlink ==> do
            cnt <- deref led_cnt
            -- toggle every 200ms
            when (cnt ==? 4) $ do
              store led_toggle true
              store led_cnt (0 :: Uint8)

            led_cnt += 1

        , ledSt ==? ledFlashSingle ==> countFlashes 1
        , ledSt ==? ledFlashDouble ==> countFlashes 2
        , ledSt ==? ledFlashTriple ==> countFlashes 3
        ]

      lt <- deref led_toggle
      when lt $ do
        ifte_ isOn
          (do
             led_off
             store led_is_on false
          )
          (do
             led_on
             store led_is_on true
          )

  return ledIn

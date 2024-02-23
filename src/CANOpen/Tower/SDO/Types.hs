{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CANOpen.Tower.SDO.Types where

import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tower
import CANOpen.Tower.Types

[ivory|
 bitdata SDOClientCommandSpecifier :: Bits 3
   = ccs_download_segment as 0b000
   | ccs_download_init as 0b001
   | ccs_upload_init as 0b010
   | ccs_upload_segment as 0b011
   | ccs_abort as 0b100
   |]

[ivory|
 bitdata SDOServerCommandSpecifier :: Bits 3
   = scs_upload_segment as 0b000
   | scs_download_segment as 0b001
   | scs_upload_init as 0b010
   | scs_download_init as 0b011
   | scs_abort as 0b100
|]

newtype SDOError =
  SDOError { unSDOError :: Uint32
           } deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)


sdoNoError :: SDOError
sdoNoError = SDOError 0x0

-- Toggle bit not alternated
sdoToggleNotAlternated :: SDOError
sdoToggleNotAlternated = SDOError 0x05030000

-- SDO protocol timed out
sdoTimeout :: SDOError
sdoTimeout = SDOError 0x05040000

-- Client/server command specifier not valid or unknown
sdoCommandSpecifierInvalid :: SDOError
sdoCommandSpecifierInvalid = SDOError 0x05040001

-- Invalid block size (block mode only)
sdoInvalidBlockSize :: SDOError
sdoInvalidBlockSize = SDOError 0x05040002

-- Invalid sequence number (block mode only)
sdoInvalidSequenceNumber :: SDOError
sdoInvalidSequenceNumber = SDOError 0x05040003

-- CRC error (block mode only)
sdoCRCError :: SDOError
sdoCRCError = SDOError 0x05040004

-- Out of memory
sdoOOM :: SDOError
sdoOOM = SDOError 0x05040005

-- Unsupported access to an object
sdoUnsupportedAccess :: SDOError
sdoUnsupportedAccess = SDOError 0x06010000

-- Attempt to read write-only attribute
sdoAttemptedWORead :: SDOError
sdoAttemptedWORead = SDOError 0x06010001

-- Attempt to write read-only attribute
sdoAttemptedROWrite :: SDOError
sdoAttemptedROWrite = SDOError 0x06010002

-- Object does not exist
sdoNotFound :: SDOError
sdoNotFound = SDOError 0x06020000

-- Object cannot be mapped to the PDO.
sdoPDOMappingNotAllowed :: SDOError
sdoPDOMappingNotAllowed = SDOError 0x06040041

-- The number and length of the objects to be mapped would exceed PDO length
sdoPDOMappingTooLarge :: SDOError
sdoPDOMappingTooLarge = SDOError 0x06040042

-- General parameter incompatibility reason
sdoGeneralParameterIncompatibility :: SDOError
sdoGeneralParameterIncompatibility = SDOError 0x06040043

-- General internal incompatibility in the device
sdoGeneralInternalIncompatibility :: SDOError
sdoGeneralInternalIncompatibility = SDOError 0x06040047

-- Access failed due to an hardware error
sdoHWError :: SDOError
sdoHWError = SDOError 0x06060000

-- Data type does not match, length of service parameter does not match
sdoSizeMismatch :: SDOError
sdoSizeMismatch = SDOError 0x06070010

-- Data type does not match, length of service parameter too high
sdoSizeMismatchParamHigh :: SDOError
sdoSizeMismatchParamHigh = SDOError 0x06070012

-- Data type does not match, length of service parameter too low
sdoSizeMismatchParamLow :: SDOError
sdoSizeMismatchParamLow = SDOError 0x06070013

-- Sub-index does not exist
sdoSubindexNotFound :: SDOError
sdoSubindexNotFound = SDOError 0x06090011

-- Value range of parameter exceeded (only for write access).
sdoValueRangeExceeded :: SDOError
sdoValueRangeExceeded = SDOError 0x06090030

-- Value of parameter written too high
sdoValueTooHigh :: SDOError
sdoValueTooHigh = SDOError 0x06090031

-- Value of parameter written too low
sdoValueTooLow :: SDOError
sdoValueTooLow = SDOError 0x06090032

-- Maximum value is less than minimum value
sdoMaxLessThanMin :: SDOError
sdoMaxLessThanMin = SDOError 0x06090036

-- General error
sdoGeneralError :: SDOError
sdoGeneralError = SDOError 0x08000000

-- Data cannot be transferred or stored to the application
sdoCannotTransferOrStore :: SDOError
sdoCannotTransferOrStore = SDOError 0x08000020

-- Data cannot be transferred or stored to the application
-- beacuse of local control
sdoCannotTransferOrStoreLocalControl :: SDOError
sdoCannotTransferOrStoreLocalControl = SDOError 0x08000021

-- Data cannot be transferred or stored to the application
-- beacuse of the present device state
sdoCannotTransferOrStoreDevState :: SDOError
sdoCannotTransferOrStoreDevState = SDOError 0x08000022

-- Object dictionary dynamic generation fails or no object dictionary is
-- present (e.g. object dictionary is generated from file and generation fails
-- because of an file error).
-- 0x08000023
-- ^^ not needed, we don't generate object dictionary dynamically

--dictErrToSDOErr :: GetReturn eff ~ 'Returns SDOError => DictError -> Ivory eff ()
dictErrToSDOErr :: Def ('[DictError] :-> SDOError)
dictErrToSDOErr = proc "dictErrToSDOErr" $ \x -> body $ do
  cond_ [
      x ==? noError ==> ret sdoNoError
    , x ==? notFound ==> ret sdoNotFound
    , x ==? readOnly ==> ret sdoAttemptedROWrite
    , x ==? writeOnly ==> ret sdoAttemptedWORead
    , x ==? sizeMismatch ==> ret sdoSizeMismatch
    , x ==? sizeMismatchParamHigh ==> ret sdoSizeMismatchParamHigh
    , x ==? sizeMismatchParamLow ==> ret sdoSizeMismatchParamLow
    , x ==? subindexNotFound ==> ret sdoSubindexNotFound
    , true ==> do
        comment "invalid conversion from DictError to SDOError"
        assert (x /=? noError)
    ]
  ret $ SDOError 0xDEAD

sdoTypes :: Module
sdoTypes = package "sdoTypes" $ do
  incl dictErrToSDOErr

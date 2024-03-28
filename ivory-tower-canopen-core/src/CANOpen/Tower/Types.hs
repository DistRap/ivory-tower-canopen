{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CANOpen.Tower.Types where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower

newtype DictError =
  DictError { unDictError :: Uint8
            } deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

noError :: DictError
noError = DictError 0

notFound :: DictError
notFound = DictError 1

readOnly :: DictError
readOnly = DictError 2

writeOnly :: DictError
writeOnly = DictError 3

sizeMismatch :: DictError
sizeMismatch = DictError 4

sizeMismatchParamHigh :: DictError
sizeMismatchParamHigh = DictError 5

sizeMismatchParamLow :: DictError
sizeMismatchParamLow = DictError 6

subindexNotFound :: DictError
subindexNotFound = DictError 7

unhandled :: DictError
unhandled = DictError 128

[ivory|
 struct mux
  { addr :: Stored Uint16
  ; sub  :: Stored Uint8
  }

 string struct PackBuffer 128

 struct muxpack
  { mp_mux :: Struct mux
  ; mp_buf :: PackBuffer
  }

 struct setresult
  { setres_ok :: Stored IBool
  ; setres_error :: Stored DictError
  ; setres_mux :: Struct mux
  }

 struct getresult
  { getres_ok :: Stored IBool
  ; getres_error :: Stored DictError
  ; getres_mux :: Struct mux
  ; getres_buf :: PackBuffer
  }
|]

data ObjDict =
  ObjDict
    { objdict_init    :: ChanInput ('Stored IBool)
    , objdict_get_in  :: ChanInput ('Struct "mux")
    , objdict_get_out :: ChanOutput ('Struct "getresult")
    , objdict_set_in  :: ChanInput ('Struct "muxpack")
    , objdict_set_out :: ChanOutput ('Struct "setresult")
    }

dictTypes :: Module
dictTypes = package "dict_types" $ do
  defStringType (Proxy :: Proxy PackBuffer)

  defStruct (Proxy :: Proxy "mux")
  defStruct (Proxy :: Proxy "muxpack")
  defStruct (Proxy :: Proxy "getresult")
  defStruct (Proxy :: Proxy "setresult")

  depend serializeModule
  wrappedPackMod muxWrapper

dictTowerDeps :: Tower e ()
dictTowerDeps = do
  towerDepends dictTypes
  towerModule dictTypes

muxWrapper :: WrappedPackRep ('Struct "mux")
muxWrapper = wrapPackRep "mux" $
  packStruct
  [ packLabel addr
  , packLabel sub
  ]

instance Packable ('Struct "mux") where
  packRep = wrappedPackRep muxWrapper

packsize :: (Packable a) => ConstRef s a -> Sint32
packsize = packsize' packRep

packsize' :: PackRep a -> ConstRef s a -> Sint32
packsize' rep _ = fromIntegral $ packSize rep

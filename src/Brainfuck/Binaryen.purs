module Brainfuck.Binaryen where

import Prelude

import Effect (Effect)
import Node.Buffer (Buffer)

foreign import data WasmBinary :: Type
foreign import data WasmExpr :: Type
foreign import data WasmModule :: Type
foreign import data WasmType :: Type
foreign import data WasmCellSize :: Type

foreign import newModule :: Effect WasmModule
foreign import setMemory :: WasmModule -> Int -> Int -> Effect Unit
foreign import addMemoryExport :: WasmModule -> String -> Effect Unit

foreign import nopExpr :: WasmModule -> WasmExpr

foreign import localGet :: WasmModule -> Int -> WasmExpr
foreign import localSet :: WasmModule -> Int -> WasmExpr -> WasmExpr
foreign import localTee :: WasmModule -> Int -> WasmExpr -> WasmExpr

foreign import constExpr :: WasmModule -> Int -> WasmExpr
foreign import addExpr :: WasmModule -> WasmExpr -> WasmExpr -> WasmExpr
foreign import subExpr :: WasmModule -> WasmExpr -> WasmExpr -> WasmExpr
foreign import mulExpr :: WasmModule -> WasmExpr -> WasmExpr -> WasmExpr
foreign import divExpr :: WasmModule -> WasmExpr -> WasmExpr -> WasmExpr
foreign import loadExpr :: WasmModule -> WasmCellSize -> WasmExpr -> Int -> WasmExpr
foreign import storeExpr :: WasmModule -> WasmCellSize -> WasmExpr -> Int -> WasmExpr -> WasmExpr
foreign import callExpr :: WasmModule -> String -> Array WasmExpr -> WasmType -> WasmExpr
foreign import dropExpr :: WasmModule -> WasmExpr -> WasmExpr

foreign import i32Type :: WasmType
foreign import noneType :: WasmType
foreign import createType :: Array WasmType -> WasmType

foreign import ifExpr :: WasmModule -> WasmExpr -> WasmExpr -> WasmExpr
foreign import blockExpr :: WasmModule -> Array WasmExpr -> WasmExpr

foreign import loopExpr :: WasmModule -> String -> WasmExpr -> WasmExpr
foreign import brExpr :: WasmModule -> String -> WasmExpr
foreign import returnExpr :: WasmModule -> WasmExpr

foreign import cell0 :: WasmCellSize
foreign import cell1 :: WasmCellSize
foreign import cell2 :: WasmCellSize

foreign import getAlign :: WasmCellSize -> Int

foreign import addFunction
  :: WasmModule
  -> String -- Function name
  -> WasmType -- Params
  -> WasmType -- Results
  -> Array WasmType -- Locals
  -> WasmExpr -- Body
  -> Effect Unit

foreign import addFunctionImport
  :: WasmModule
  -> String -- Internal name
  -> String -- External module name
  -> String -- External base name
  -> WasmType -- Params
  -> WasmType -- Results
  -> Effect Unit

foreign import addFunctionExport
  :: WasmModule
  -> String -- Internal name
  -> String -- External name
  -> Effect Unit

foreign import setStart
  :: WasmModule
  -> String -- Function name
  -> Effect Unit

foreign import optimize :: WasmModule -> Effect Unit
foreign import optimizeFunction :: WasmModule -> String -> Effect Unit
foreign import validate :: WasmModule -> Effect Unit
foreign import emitBinary :: WasmModule -> Effect WasmBinary

foreign import emitText :: WasmModule -> Effect String

foreign import setOptimizeLevel :: WasmModule -> Int -> Effect Unit

foreign import setShrinkLevel :: WasmModule -> Int -> Effect Unit

foreign import emitStackIR :: WasmModule -> Effect String

foreign import binaryToBuffer :: WasmBinary -> Effect Buffer

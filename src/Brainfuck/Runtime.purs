module Brainfuck.Runtime where

import Prelude

import Brainfuck.Binaryen (WasmBinary)
import Data.String (CodePoint)
import Effect (Effect)

foreign import data WasmInstance :: Type

foreign import newInstance
  :: String
  -> String
  -> String
  -> Effect CodePoint
  -> (CodePoint -> Effect Unit)
  -> WasmBinary
  -> Effect WasmInstance

runBinary
  :: { importModule :: String
     , inputFunction :: String
     , outputFunction :: String
     , mainFunction :: String
     }
  -> Effect CodePoint
  -> (CodePoint -> Effect Unit)
  -> WasmBinary
  -> Effect Unit
runBinary { importModule, inputFunction, outputFunction, mainFunction } input output binary = do
  void $ newInstance importModule inputFunction outputFunction input output binary

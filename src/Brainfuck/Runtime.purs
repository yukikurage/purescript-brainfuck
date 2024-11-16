module Brainfuck.Runtime where

import Prelude

import Brainfuck.Binaryen (WasmBinary)
import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data WasmInstance :: Type

foreign import newInstance
  :: WasmBinary
  -> Effect (Promise Unit)

runBinary
  :: WasmBinary
  -> Aff Unit
runBinary binary = do
  toAffE $ newInstance binary

module Brainfuck.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)

data Operation
  = Increment
  | Decrement
  | MoveRight
  | MoveLeft
  | Output
  | Input
  | Loop AST

derive instance genericOperation :: Generic Operation _

instance showOperation :: Show Operation where
  show a = genericShow a

type AST = List Operation

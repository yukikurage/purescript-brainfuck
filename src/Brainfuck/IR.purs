module Brainfuck.IR where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)

type Offset = Int

data LeftValue = ToMemory Offset

derive instance Generic LeftValue _

instance Show LeftValue where
  show a = genericShow a

data RightValue
  = FromMemory Offset
  | Constant Int
  | Add RightValue RightValue
  | Sub RightValue RightValue
  | Mul RightValue RightValue
  | Div RightValue RightValue
  | Input

derive instance Generic RightValue _

instance Show RightValue where
  show a = genericShow a

data Statement
  = Assignment LeftValue RightValue
  | Loop IR -- Loop until the break called
  | Break
  | If RightValue IR IR -- If the value is not zero, execute the first IR, otherwise execute the second IR
  | Output RightValue
  | MovePointer RightValue

derive instance Generic Statement _

instance Show Statement where
  show a = genericShow a

type IR = List Statement

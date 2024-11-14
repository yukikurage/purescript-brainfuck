module Brainfuck.IR where

import Prelude

import Data.Array (concatMap, fromFoldable, replicate)
import Data.Generic.Rep (class Generic)
import Data.List (List, fold)
import Data.Show.Generic (genericShow)
import Data.String as String

type Offset = Int

data LeftValue = ToMemory Offset

derive instance Generic LeftValue _

instance Show LeftValue where
  show = case _ of
    ToMemory offset -> "[" <> show offset <> "]"

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
  show = case _ of
    FromMemory offset -> "[" <> show offset <> "]"
    Constant value -> show value
    Add left right -> "(" <> show left <> " + " <> show right <> ")"
    Sub left right -> "(" <> show left <> " - " <> show right <> ")"
    Mul left right -> "(" <> show left <> " * " <> show right <> ")"
    Div left right -> "(" <> show left <> " / " <> show right <> ")"
    Input -> "input"

data Statement
  = Assignment LeftValue RightValue
  | Loop IR -- Loop until the break called
  | If RightValue IR -- If the value is not zero, execute the first IR, otherwise execute the second IR
  | Output RightValue
  | MovePointer RightValue

derive instance Generic Statement _

showStatement :: Statement -> Array String
showStatement = case _ of
  Assignment left right -> [ show left <> " <- " <> show right <> "" ]
  Loop ir -> [ "loop: " ] <> showIR 2 ir
  If cond tExp -> [ "if " <> show cond <> ":" ] <> showIR 2 tExp
  Output right -> [ "output " <> show right ]
  MovePointer right -> [ "move " <> show right ]

type IR = List Statement

showIR :: Int -> IR -> Array String
showIR space ir = concatMap (\statement -> map (\x -> fold (replicate space " ") <> x) $ showStatement statement) $ fromFoldable ir

viewIR :: IR -> String
viewIR = String.joinWith "\n" <<< showIR 0

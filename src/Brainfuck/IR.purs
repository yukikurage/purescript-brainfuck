module Brainfuck.IR where

import Prelude

import Data.Array (concatMap, fromFoldable, replicate)
import Data.Generic.Rep (class Generic)
import Data.List (List, fold)
import Data.String as String

type Offset = Int

data RightValue
  = FromMemory Offset
  | Constant Int
  | Add RightValue RightValue
  | Sub RightValue RightValue
  | Mul RightValue RightValue
  | Div RightValue RightValue

derive instance Eq RightValue

derive instance Generic RightValue _

instance Show RightValue where
  show = case _ of
    FromMemory offset -> "[" <> show offset <> "]"
    Constant value -> show value
    Add left right -> "(" <> show left <> " + " <> show right <> ")"
    Sub left right -> "(" <> show left <> " - " <> show right <> ")"
    Mul left right -> "(" <> show left <> " * " <> show right <> ")"
    Div left right -> "(" <> show left <> " / " <> show right <> ")"

data Statement
  -- Assignment n, Addition n, Substraction n の RightValue には FromMemory n が入っていないことが保証されていなければならない
  -- これによって合成が容易になる
  -- 例えば Assignment を 2 回行うときは後者を残せばよい
  -- また一番外側はできる限り (Constant n), (Constant n) +, (Constant n) * のどれかになるように作る
  = Assignment Offset RightValue
  | Addition Offset RightValue
  | Loop Boolean IR -- Loop until the break called
  | If RightValue IR -- If the value is not zero, execute the IR
  | Output Offset
  | MovePointer RightValue
  | Input Offset

derive instance Generic Statement _

showStatement :: Statement -> Array String
showStatement = case _ of
  Assignment left right -> [ "[" <> show left <> "] = " <> show right ]
  Addition left right -> [ "[" <> show left <> "] += " <> show right ]
  Loop _ ir -> [ "loop: " ] <> showIR 2 ir
  If cond tExp -> [ "if " <> show cond <> ":" ] <> showIR 2 tExp
  Output right -> [ "output " <> show right ]
  MovePointer right -> [ "move " <> show right ]
  Input offset -> [ "input " <> show offset ]

type IR = List Statement

showIR :: Int -> IR -> Array String
showIR space ir = concatMap (\statement -> map (\x -> fold (replicate space " ") <> x) $ showStatement statement) $ fromFoldable ir

viewIR :: IR -> String
viewIR = String.joinWith "\n" <<< showIR 0

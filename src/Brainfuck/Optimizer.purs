module Brainfuck.Optimizer where

import Prelude

import Brainfuck.IR (IR, Offset, Statement)

-- isReferred :: Offset -> Statement -> Boolean
-- isReferred

-- | IR -> IR
optimize :: { lookahead :: Int } -> IR -> IR
optimize { lookahead } ir = ir

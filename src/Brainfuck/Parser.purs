module Brainfuck.Parser where

import Prelude

import Brainfuck.AST (AST)
import Brainfuck.AST as AST
import Control.Monad.RWS (modify_)
import Control.Monad.State (State, evalState, get)
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)

parse :: String -> AST
parse code = evalState go 0
  where
  popChar :: State Int (Maybe Char)
  popChar = do
    idx <- get
    modify_ (_ + 1)
    pure (charAt idx code)

  go :: State Int AST
  go = do
    char <- popChar

    case char of
      Just '+' -> (AST.Increment : _) <$> go
      Just '-' -> (AST.Decrement : _) <$> go
      Just '>' -> (AST.MoveRight : _) <$> go
      Just '<' -> (AST.MoveLeft : _) <$> go
      Just '.' -> (AST.Output : _) <$> go
      Just ',' -> (AST.Input : _) <$> go
      Just '[' -> do
        body <- go
        (AST.Loop body : _) <$> go
      Just ']' -> pure mempty
      Just _ -> go
      Nothing -> pure mempty

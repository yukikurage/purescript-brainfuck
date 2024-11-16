module Brainfuck.Parser where

import Prelude

import Brainfuck.AST (AST)
import Brainfuck.AST as AST
import Control.Monad.RWS (modify_)
import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.Rec.Class as R
import Control.Monad.State (State, evalState, get)
import Data.List (snoc)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)

parse :: String -> AST
parse code = evalState (go mempty) 0
  where
  popChar :: State Int (Maybe Char)
  popChar = do
    idx <- get
    modify_ (_ + 1)
    pure (charAt idx code)

  go :: AST -> State Int AST
  go = tailRecM \acc -> do
    char <- popChar

    case char of
      Just '+' -> pure $ R.Loop (acc `snoc` AST.Increment)
      Just '-' -> pure $ R.Loop (acc `snoc` AST.Decrement)
      Just '>' -> pure $ R.Loop (acc `snoc` AST.MoveRight)
      Just '<' -> pure $ R.Loop (acc `snoc` AST.MoveLeft)
      Just '.' -> pure $ R.Loop (acc `snoc` AST.Output)
      Just ',' -> pure $ R.Loop (acc `snoc` AST.Input)
      Just '$' -> do
        char' <- popChar
        case char' of
          Just '[' -> do
            body <- go mempty
            pure $ R.Loop (acc `snoc` AST.Loop false body)
          _ -> pure $ R.Loop acc
      Just '[' -> do
        body <- go mempty
        pure $ R.Loop (acc `snoc` AST.Loop true body)
      Just ']' -> pure $ R.Done acc
      Just _ -> pure $ R.Loop acc
      Nothing -> pure $ R.Done acc

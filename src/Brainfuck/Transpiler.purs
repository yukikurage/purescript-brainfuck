module Brainfuck.Transpiler where

import Prelude

import Brainfuck.AST (AST)
import Brainfuck.AST as AST
import Brainfuck.IR (IR)
import Brainfuck.IR as IR
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

data CellOperation = Add Int | Set Int

derive instance Eq CellOperation

appendOp :: CellOperation -> CellOperation -> CellOperation
appendOp (Add a) (Add b) = Add (a + b)
appendOp (Set a) (Add b) = Set (a + b)
appendOp (Add _) (Set b) = Set b
appendOp (Set _) (Set b) = Set b

instance Semigroup CellOperation where
  append = appendOp

instance Monoid CellOperation where
  mempty = Add 0

type TranspileState = { knownVals :: Map Int Int, deferredOps :: Map Int CellOperation, deferredPointer :: Int }

transpile :: Int -> AST -> IR
transpile cellCount ast =
  let
    -- | Try to load the raw value of a cell, using the known values and deferred operations
    tryLoad :: Int -> StateT TranspileState (Writer IR) (Maybe Int)
    tryLoad idx = do
      { knownVals, deferredOps } <- get
      pure case Map.lookup idx knownVals /\ Map.lookup idx deferredOps of
        Just val /\ Just (Add offset) -> Just (val + offset)
        Nothing /\ Just (Set val) -> Just val
        Just val /\ Nothing -> Just val
        _ -> Nothing

    -- | Force the deferred operations
    forceOp :: Int -> StateT TranspileState (Writer IR) Unit
    forceOp idx = do
      { knownVals, deferredOps, deferredPointer } <- get
      case Map.lookup idx knownVals /\ Map.lookup idx deferredOps of
        Just val /\ Just op -> do
          let
            newVal = case op of
              Add offset -> val + offset
              Set v -> v
            newKnownVals = Map.insert idx newVal knownVals
            newDeferredOps = Map.delete idx deferredOps
          put { knownVals: newKnownVals, deferredOps: newDeferredOps, deferredPointer }
          tell $ List.singleton $ IR.Assignment (IR.ToMemory idx) (IR.Constant newVal)
        _ /\ Just (Add offset) -> do
          let
            newDeferredOps = Map.delete idx deferredOps
          put { knownVals, deferredOps: newDeferredOps, deferredPointer }
          tell $ List.singleton $ IR.Assignment (IR.ToMemory idx) (IR.Add (IR.FromMemory idx) (IR.Constant offset))
        _ /\ Just (Set val) -> do
          let
            newKnownVals = Map.insert idx val knownVals
            newDeferredOps = Map.delete idx deferredOps
          put { knownVals: newKnownVals, deferredOps: newDeferredOps, deferredPointer }
          tell $ List.singleton $ IR.Assignment (IR.ToMemory idx) (IR.Constant val)
        _ -> pure unit

    forceAllAndPointer :: StateT TranspileState (Writer IR) Unit
    forceAllAndPointer = do
      { deferredOps, deferredPointer } <- get
      forWithIndex_ deferredOps \idx op -> case op of
        Add offset -> tell $ List.singleton $ IR.Assignment
          (IR.ToMemory idx)
          (IR.Add (IR.FromMemory idx) (IR.Constant offset))
        Set val -> tell $ List.singleton $ IR.Assignment
          (IR.ToMemory idx)
          (IR.Constant val)
      tell $ List.singleton $ IR.MovePointer (IR.Constant deferredPointer)
      put { knownVals: Map.empty, deferredOps: Map.empty, deferredPointer: 0 }

    forgetAt :: Int -> StateT TranspileState (Writer IR) Unit
    forgetAt idx = do
      { knownVals, deferredOps, deferredPointer } <- get
      let
        newKnownVals = Map.delete idx knownVals
        newDeferredOps = Map.delete idx deferredOps
      put { knownVals: newKnownVals, deferredOps: newDeferredOps, deferredPointer }
      pure unit

    deferOp :: Int -> CellOperation -> StateT TranspileState (Writer IR) Unit
    deferOp idx op = do
      { knownVals, deferredOps, deferredPointer } <- get
      let
        newDeferredOps = Map.insertWith append idx op deferredOps
      put { knownVals, deferredOps: newDeferredOps, deferredPointer }
      pure unit

    deferPointerMove :: Int -> StateT TranspileState (Writer IR) Unit
    deferPointerMove offset = do
      { knownVals, deferredOps, deferredPointer } <- get
      let
        newDeferredPointer = deferredPointer + offset
      put { knownVals, deferredOps, deferredPointer: newDeferredPointer }
      pure unit

    go :: AST -> StateT TranspileState (Writer IR) Unit
    go a = tailRecM goStep a

    goStep :: AST -> StateT TranspileState (Writer IR) (Step AST Unit)
    goStep List.Nil = pure $ Done unit
    goStep (h : t) = do
      { deferredPointer } <- get
      case h of
        AST.Increment -> do
          deferOp deferredPointer $ Add 1
        AST.Decrement -> do
          deferOp deferredPointer $ Add (-1)
        AST.MoveRight -> do
          deferPointerMove 1
        AST.MoveLeft -> do
          deferPointerMove (-1)
        AST.Output -> do
          res <- tryLoad deferredPointer
          case res of
            Just val -> tell $ List.singleton $ IR.Output (IR.Constant val)
            Nothing -> do
              forceOp deferredPointer -- Before IR.FromMemory
              tell $ List.singleton $ IR.Output (IR.FromMemory deferredPointer)
        AST.Input -> do
          forgetAt deferredPointer
          tell $ List.singleton $ IR.Assignment (IR.ToMemory deferredPointer) IR.Input
        AST.Loop body -> do
          let
            loopState /\ loopIR =
              execStateT (go body) { knownVals: Map.empty, deferredOps: Map.empty, deferredPointer: 0 }
                # runWriter
          currentTryVal <- tryLoad deferredPointer
          if
            loopState.deferredPointer == 0 && List.length loopIR == 0 &&
              Map.lookup 0 loopState.deferredOps == Just (Add (-1)) then case currentTryVal of
            Just current -> do
              -- 最適化 1
              forWithIndex_ loopState.deferredOps \idx op -> when (idx /= 0) do
                deferOp (deferredPointer + idx)
                  ( case op of
                      Add offset -> Add $ offset * current
                      Set val -> if current /= 0 then Set val else mempty
                  )
              deferOp deferredPointer $ Set 0
            Nothing -> do
              -- 最適化 2
              forceOp deferredPointer
              forWithIndex_ loopState.deferredOps \idx op -> when (idx /= 0) case op of
                Add offset -> do
                  forceOp (deferredPointer + idx)
                  tell $ List.singleton $ IR.Assignment
                    (IR.ToMemory (deferredPointer + idx))
                    ( IR.Add
                        (IR.FromMemory (deferredPointer + idx))
                        ( IR.Mul
                            (IR.FromMemory deferredPointer)
                            (IR.Constant offset)
                        )
                    )
                Set val -> do
                  forceOp (deferredPointer + idx)
                  tell $ List.singleton $ IR.If
                    (IR.FromMemory deferredPointer)
                    (List.singleton $ IR.Assignment (IR.ToMemory (deferredPointer + idx)) (IR.Constant val))
                    mempty
              deferOp deferredPointer $ Set 0
          else do
            forceAllAndPointer
            -- 最後まで進める
            let
              _ /\ remainingLoopIR = execStateT forceAllAndPointer loopState # runWriter
              resultLoopIR = loopIR <> remainingLoopIR <> List.singleton IR.Break
            tell $ List.singleton $ IR.Loop $ List.singleton
              ( IR.If (IR.FromMemory 0) resultLoopIR mempty
              )
      pure $ Loop t

    _ /\ ir =
      execStateT
        (go ast)
        { knownVals: Map.fromFoldable $ map (\idx -> idx /\ 0) (0 .. (cellCount - 1))
        , deferredOps: Map.empty
        , deferredPointer: 0
        }
        # runWriter
  in
    ir

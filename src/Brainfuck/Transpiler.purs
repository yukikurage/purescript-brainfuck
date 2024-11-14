module Brainfuck.Transpiler where

import Prelude

import Brainfuck.AST (AST)
import Brainfuck.AST as AST
import Brainfuck.IR (IR)
import Brainfuck.IR as IR
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, execStateT, get, modify_, put)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (any, (..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (forWithIndex)
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

transpile :: Int -> Int -> AST -> IR
transpile lookahead cellCount ast =
  let
    -- | Try to load the raw value of a cell, using the known values and deferred operations
    tryLoad :: Int -> StateT TranspileState (Writer IR) (Maybe Int)
    tryLoad idx = do
      { knownVals, deferredOps } <- get
      pure case Map.lookup idx knownVals /\ Map.lookup idx deferredOps of
        Just val /\ Just (Add offset) -> Just (val + offset)
        _ /\ Just (Set val) -> Just val
        Just val /\ Nothing -> Just val
        _ -> Nothing

    -- | Force the deferred operations
    forceOp :: Int -> StateT TranspileState (Writer IR) Unit
    forceOp idx = do
      { knownVals, deferredOps, deferredPointer } <- get
      case Map.lookup idx knownVals /\ Map.lookup idx deferredOps of
        Just val /\ Just (Set v) | val == v -> do
          let
            newDeferredOps = Map.delete idx deferredOps
          put { knownVals, deferredOps: newDeferredOps, deferredPointer }
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
      { knownVals, deferredOps, deferredPointer } <- get
      forWithIndex_ deferredOps \idx op -> case Map.lookup idx knownVals /\ op of
        Just val /\ op -> do
          let
            newVal = case op of
              Add offset -> val + offset
              Set v -> v
            newKnownVals = Map.insert idx newVal knownVals
          -- newDeferredOps = Map.delete idx deferredOps
          put { knownVals: newKnownVals, deferredOps, deferredPointer }
          tell $ List.singleton $ IR.Assignment (IR.ToMemory idx) (IR.Constant newVal)
        _ /\ Add offset -> do
          -- let
          -- newDeferredOps = Map.delete idx deferredOps
          put { knownVals, deferredOps, deferredPointer }
          tell $ List.singleton $ IR.Assignment (IR.ToMemory idx) (IR.Add (IR.FromMemory idx) (IR.Constant offset))
        _ /\ Set val -> do
          let
            newKnownVals = Map.insert idx val knownVals
            newDeferredOps = Map.delete idx deferredOps
          put { knownVals: newKnownVals, deferredOps: newDeferredOps, deferredPointer }
          tell $ List.singleton $ IR.Assignment (IR.ToMemory idx) (IR.Constant val)

      when (deferredPointer /= 0) $ tell $ List.singleton $ IR.MovePointer (IR.Constant deferredPointer)
      { knownVals } <- get
      -- key をずらす
      put { knownVals: Map.fromFoldable $ Map.values $ (mapWithIndex \i a -> (i - deferredPointer) /\ a) knownVals, deferredOps: Map.empty, deferredPointer: 0 }

    forgetAt :: Int -> StateT TranspileState (Writer IR) Unit
    forgetAt idx = do
      { knownVals, deferredOps, deferredPointer } <- get
      let
        newKnownVals = Map.delete idx knownVals
        newDeferredOps = Map.delete idx deferredOps
      put { knownVals: newKnownVals, deferredOps: newDeferredOps, deferredPointer }
      pure unit

    addKnownVal :: Int -> Int -> StateT TranspileState (Writer IR) Unit
    addKnownVal idx val = do
      { knownVals, deferredOps, deferredPointer } <- get
      let
        newKnownVals = Map.insert idx val knownVals
      put { knownVals: newKnownVals, deferredOps, deferredPointer }
      pure unit

    forgetAllKnown :: StateT TranspileState (Writer IR) Unit
    forgetAllKnown = do
      { deferredOps, deferredPointer } <- get
      put { knownVals: Map.empty, deferredOps, deferredPointer }
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
                  forgetAt (deferredPointer + idx)
                  tell $ List.singleton $ IR.Assignment
                    (IR.ToMemory (deferredPointer + idx))
                    ( case offset of
                        1 -> IR.Add (IR.FromMemory (deferredPointer + idx)) (IR.FromMemory (deferredPointer))
                        (-1) -> IR.Sub (IR.FromMemory (deferredPointer + idx)) (IR.FromMemory (deferredPointer))
                        _ ->
                          ( IR.Add
                              (IR.FromMemory (deferredPointer + idx))
                              ( IR.Mul
                                  (IR.FromMemory deferredPointer)
                                  (IR.Constant offset)
                              )
                          )
                    )

                Set val -> do
                  forceOp (deferredPointer + idx)
                  forgetAt (deferredPointer + idx)
                  tell $ List.singleton $ IR.If
                    (IR.FromMemory deferredPointer)
                    (List.singleton $ IR.Assignment (IR.ToMemory (deferredPointer + idx)) (IR.Constant val))
              deferOp deferredPointer $ Set 0
          else if
            loopState.deferredPointer == 0 && List.length loopIR == 0 &&
              Map.lookup 0 loopState.deferredOps == Just (Set 0) then case currentTryVal of
            Just 0 -> pure unit -- false
            Just _ -> do
              forWithIndex_ loopState.deferredOps \idx op -> when (idx /= 0) do
                deferOp (deferredPointer + idx)
                  ( case op of
                      Add offset -> Add offset
                      Set val -> Set val
                  )
              deferOp deferredPointer $ Set 0
            Nothing -> do
              forceOp deferredPointer
              forWithIndex_ loopState.deferredOps
                \idx _ -> do
                  forceOp (deferredPointer + idx)
                  forgetAt (deferredPointer + idx)
              tell $ List.singleton $ IR.If
                (IR.FromMemory deferredPointer)
                ( List.fromFoldable $ mapWithIndex
                    ( \idx op -> IR.Assignment (IR.ToMemory (deferredPointer + idx))
                        ( case op of
                            Add offset -> IR.Add (IR.FromMemory (deferredPointer + idx)) (IR.FromMemory (deferredPointer))
                            Set val -> IR.Constant val
                        )
                    )
                    loopState.deferredOps
                )
              deferOp deferredPointer $ Set 0
          else do
            -- ミスったら戻る用の状態
            currentState <- get

            -- 何回かループを実行して止まることを期待
            -- あんまりインパクトない？
            res <-
              if List.length loopIR == 0 then do
                let
                  loop :: Int -> StateT TranspileState (Writer IR) Boolean
                  loop 0 = pure false
                  loop n = do
                    { deferredPointer: nowPointer } <- get
                    val <- tryLoad nowPointer

                    case val of
                      Just 0 -> pure true
                      Just _ -> do
                        forWithIndex_ loopState.deferredOps \idx op -> deferOp (deferredPointer + idx) op
                        deferPointerMove loopState.deferredPointer
                        loop (n - 1)
                      Nothing -> pure false

                loop lookahead
              else pure false
            unless res do
              put currentState
              forceAllAndPointer
              -- 最後まで進める
              let
                _ /\ remainingLoopIR = execStateT forceAllAndPointer loopState # runWriter
                resultLoopIR = loopIR <> remainingLoopIR
              tell $ List.singleton $ IR.Loop resultLoopIR
              -- ループ実行後に knownVals が変化する
              if List.length loopIR == 0 && loopState.deferredPointer == 0 then do
                -- IR がない場合は完全に変化位置を予想可能
                let
                  -- ループによって不明になるインデックス
                  isUnknown :: Int -> Boolean
                  isUnknown idx =
                    any
                      (\opIdx -> let diff = idx - opIdx in diff `mod` loopState.deferredPointer == 0 && diff `div` loopState.deferredPointer >= 0)
                      $ Map.keys loopState.deferredOps
                modify_ \s@{ knownVals } -> s { knownVals = Map.filterKeys (not isUnknown) knownVals }
              else
                forgetAllKnown
              addKnownVal 0 0

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

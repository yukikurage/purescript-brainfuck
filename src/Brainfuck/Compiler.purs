module Brainfuck.Compiler where

import Prelude

import Brainfuck.Binaryen (WasmBinary, WasmCellSize, WasmExpr, addExpr, addFunction, addFunctionExport, addFunctionImport, addMemoryExport, blockExpr, brExpr, callExpr, constExpr, divExpr, emitBinary, emitText, i32Type, ifExpr, loadExpr, localGet, localSet, loopExpr, mulExpr, newModule, noneType, optimize, returnExpr, setMemory, setOptimizeLevel, storeExpr, subExpr, validate)
import Brainfuck.IR (IR, RightValue(..), Statement(..))
import Control.Monad.State (State, evalState, get, modify_)
import Data.List (List)
import Data.List as List
import Effect (Effect)
import Effect.Console (log)

compile
  :: { cellSize :: WasmCellSize
     , importModule :: String
     , inputFunction :: String
     , outputFunction :: String
     , mainFunction :: String
     }
  -> IR
  -> Effect WasmBinary
compile { cellSize, importModule, inputFunction, outputFunction, mainFunction } ir = do
  mod <- newModule
  setMemory mod 4 4
  addMemoryExport mod "memory"

  let
    constE :: Int -> WasmExpr
    constE = constExpr mod

    addE :: WasmExpr -> WasmExpr -> WasmExpr
    addE = addExpr mod

    subE :: WasmExpr -> WasmExpr -> WasmExpr
    subE = subExpr mod

    mulE :: WasmExpr -> WasmExpr -> WasmExpr
    mulE = mulExpr mod

    divE :: WasmExpr -> WasmExpr -> WasmExpr
    divE = divExpr mod

    getPointerE :: WasmExpr
    getPointerE = localGet mod 0

    -- movePointerE :: WasmExpr -> WasmExpr
    -- movePointerE offset = localSet mod 0 (addE (localGet mod 0) (mulE offset alignE))

    movePointerE :: WasmExpr -> WasmExpr
    movePointerE offset = localSet mod 0 (addE (localGet mod 0) offset)

    loadMemoryE :: Int -> WasmExpr
    loadMemoryE offset
      | offset >= (-64) = loadExpr mod cellSize getPointerE (offset + 64)
      | otherwise = loadExpr mod cellSize (addE getPointerE (constE (offset + 64))) 0

    storeMemoryE :: Int -> WasmExpr -> WasmExpr
    storeMemoryE offset value
      | offset >= (-64) = storeExpr mod cellSize getPointerE (offset + 64) value
      | otherwise = storeExpr mod cellSize (addE getPointerE (constE (offset + 64))) 0 value

    outputE :: WasmExpr -> WasmExpr
    outputE expr = callExpr mod outputFunction [ expr ] noneType

    inputE :: WasmExpr
    inputE = callExpr mod inputFunction [] i32Type

    loopE :: String -> WasmExpr -> WasmExpr
    loopE label body = loopExpr mod label body

    blockE :: List WasmExpr -> WasmExpr
    blockE exprs = blockExpr mod $ List.toUnfoldable exprs

    ifE :: WasmExpr -> WasmExpr -> WasmExpr
    ifE = ifExpr mod

    brE :: String -> WasmExpr
    brE = brExpr mod

    mkLabel :: State Int String
    mkLabel = do
      idx <- get
      modify_ (_ + 1)
      pure $ "label" <> show idx

    compileRightValue :: RightValue -> WasmExpr
    compileRightValue = case _ of
      FromMemory offset -> loadMemoryE offset
      Constant value -> constE value
      Add left right -> addE (compileRightValue left) (compileRightValue right)
      Sub left right -> subE (compileRightValue left) (compileRightValue right)
      Mul left right -> mulE (compileRightValue left) (compileRightValue right)
      Div left right -> divE (compileRightValue left) (compileRightValue right)

    go :: String -> IR -> State Int (List WasmExpr)
    go label = case _ of
      List.Nil -> pure mempty
      st List.: ir' -> do
        h <- case st of
          Addition offset (Mul (Constant (-1)) right) -> pure $ storeMemoryE offset (subE (loadMemoryE offset) (compileRightValue right))
          Addition offset right -> pure $ storeMemoryE offset (addE (loadMemoryE offset) (compileRightValue right))
          Assignment offset right -> pure $ storeMemoryE offset (compileRightValue right)
          Loop _ body -> do
            label' <- mkLabel
            bodyE <- go label' body
            pure $ loopE label'
              ( ifE
                  (loadMemoryE 0)
                  (blockE (bodyE <> List.singleton (brE label')))
              )
          If cond tExp -> do
            tExp' <- go label tExp
            pure $ ifE (compileRightValue cond) (blockE tExp')
          Output right -> pure $ outputE (compileRightValue right)
          MovePointer right -> pure $ movePointerE (compileRightValue right)
          Input offset -> pure $ storeMemoryE offset inputE
        t <- go label ir'
        pure $ h List.: t

  let exprs = evalState (go "label" ir) 0

  addFunctionImport mod "output" importModule outputFunction i32Type noneType
  addFunctionImport mod "input" importModule inputFunction i32Type i32Type

  addFunction mod "main"
    noneType
    noneType
    [ i32Type {- Pointer -} ]
    (blockE (List.singleton (localSet mod 0 (constE 0)) <> exprs <> List.singleton (returnExpr mod)))

  addFunctionExport mod "main" mainFunction

  validate mod
  -- optimize mod

  log =<< emitText mod

  emitBinary mod

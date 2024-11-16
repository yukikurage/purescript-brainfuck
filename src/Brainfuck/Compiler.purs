module Brainfuck.Compiler where

import Prelude

import Brainfuck.Binaryen (WasmBinary, WasmCellSize, WasmExpr, addExpr, addFunction, addFunctionExport, addFunctionImport, addMemoryExport, blockExpr, brExpr, callExpr, cell0, cell2, constExpr, createType, divExpr, dropExpr, emitBinary, i32Type, ifExpr, loadExpr, localGet, localSet, localTee, loopExpr, mulExpr, newModule, noneType, optimize, returnExpr, setMemory, storeExpr, subExpr, validate)
import Brainfuck.IR (IR, Offset, RightValue(..), Statement(..))
import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.Rec.Class as Rec
import Control.Monad.State (State, evalState, get, modify_)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)

-- | Compile 最適化用
-- | pointer set の後に pointer get をする時、 tee を呼ぶ
data RightValueT
  = FromMemoryT Offset
  | ConstantT Int
  | AddT RightValueT RightValueT
  | SubT RightValueT RightValueT
  | MulT RightValueT RightValueT
  | DivT RightValueT RightValueT
  | FromMemoryTee
      RightValueT -- Pointer をどれだけ動かすかが埋め込まれる
      Offset

data StatementT
  -- Assignment n, Addition n, Substraction n の RightValue には FromMemory n が入っていないことが保証されていなければならない
  -- これによって合成が容易になる
  -- 例えば Assignment を 2 回行うときは後者を残せばよい
  -- また一番外側はできる限り (Constant n), (Constant n) +, (Constant n) * のどれかになるように作る
  = AssignmentT Offset RightValueT
  | AssignmentTee RightValueT -- ポインタの差分

      Offset
      RightValueT
  | AdditionT Offset RightValueT
  | AdditionTee RightValueT -- ポインタの差分

      Offset
      RightValueT
  | LoopT Boolean IRT -- Loop until the break called
  | IfT RightValueT IRT -- If the value is not zero, execute the IR
  | OutputT RightValueT
  | MovePointerT RightValueT
  | InputT Offset

type IRT = List StatementT

rvToRvT :: RightValue -> RightValueT
rvToRvT = case _ of
  FromMemory offset -> FromMemoryT offset
  Constant value -> ConstantT value
  Add left right -> AddT (rvToRvT left) (rvToRvT right)
  Sub left right -> SubT (rvToRvT left) (rvToRvT right)
  Mul left right -> MulT (rvToRvT left) (rvToRvT right)
  Div left right -> DivT (rvToRvT left) (rvToRvT right)

irToIrT :: IR -> IRT
irToIrT = map case _ of
  Assignment offset right -> AssignmentT offset (rvToRvT right)
  Addition offset right -> AdditionT offset (rvToRvT right)
  Loop b ir -> LoopT b (irToIrT ir)
  If cond tExp -> IfT (rvToRvT cond) (irToIrT tExp)
  Output right -> OutputT (rvToRvT right)
  MovePointer right -> MovePointerT (rvToRvT right)
  Input offset -> InputT offset

-- | MovePointerT r に次いで Statement が来た時、その Statement が Stack を使うように変形
modifyTee :: RightValueT -> StatementT -> Maybe StatementT
modifyTee diff = case _ of
  AssignmentT offset right -> Just $ AssignmentTee diff offset right
  AdditionT offset right -> Just $ AdditionTee diff offset right
  IfT (FromMemoryT offset) ir -> Just $ IfT (FromMemoryTee diff offset) ir
  OutputT (FromMemoryT offset) -> Just $ OutputT (FromMemoryTee diff offset)
  _ -> Nothing

optTee :: IRT -> IRT
optTee = case _ of
  (MovePointerT diff) : st2 : sts -> case modifyTee diff st2 of
    Just st2' -> st2' : optTee sts
    Nothing -> MovePointerT diff : (optTee (st2 : sts))
  LoopT b ir : sts -> LoopT b (optTee ir) : (optTee sts)
  IfT cond ir : sts -> IfT cond (optTee ir) : (optTee sts)
  st : sts -> st : (optTee sts)
  List.Nil -> List.Nil

compile
  :: { cellSize :: WasmCellSize
     }
  -> IR
  -> Effect WasmBinary
compile { cellSize } ir = do
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

    getPointerE :: Unit -> WasmExpr
    getPointerE _ = localGet mod 0

    teePointerE :: WasmExpr -> WasmExpr
    teePointerE = localTee mod 0

    -- movePointerE :: WasmExpr -> WasmExpr
    -- movePointerE offset = localSet mod 0 (addE (localGet mod 0) (mulE offset alignE))

    movePointerE :: WasmExpr -> WasmExpr
    movePointerE offset = localSet mod 0 (addE (localGet mod 0) offset)

    loadMemoryE :: Int -> WasmExpr
    loadMemoryE offset
      | offset >= 0 = loadExpr mod cellSize (getPointerE unit) offset
      | otherwise = loadExpr mod cellSize (addE (getPointerE unit) (constE offset)) 0

    -- | Stack に既に積まれているので getPointer をする必要がない
    loadMemoryTeeE :: WasmExpr -> Int -> WasmExpr
    loadMemoryTeeE diff offset
      | offset >= 0 = loadExpr mod cellSize (teePointerE (addE (localGet mod 0) diff)) offset
      | otherwise = loadExpr mod cellSize (addE (teePointerE (addE (localGet mod 0) diff)) (constE 64)) 0

    storeMemoryE :: Int -> WasmExpr -> WasmExpr
    storeMemoryE offset value
      | offset >= 0 = storeExpr mod cellSize (getPointerE unit) offset value
      | otherwise = storeExpr mod cellSize (addE (getPointerE unit) (constE offset)) 0 value

    -- | Stack に既に積まれているので getPointer をする必要がない
    storeMemoryTeeE :: WasmExpr -> Int -> WasmExpr -> WasmExpr
    storeMemoryTeeE diff offset value
      | offset >= 0 = storeExpr mod cellSize (teePointerE (addE (localGet mod 0) diff)) offset value
      | otherwise = storeExpr mod cellSize (addE (teePointerE (addE (localGet mod 0) diff)) (constE offset)) 0 value

    outputE :: WasmExpr -> WasmExpr
    -- outputE expr = callExpr mod outputFunction [ expr ] noneType
    outputE expr = blockExpr mod
      [ storeExpr mod cell0 (constExpr mod 16) 0 expr
      , dropExpr mod
          ( callExpr mod "fd_write"
              [ constE 1 -- stdout
              , constE 0 -- iov へのポインタ
              , constE 1 -- iov の長さ
              , constE 20 -- 適当な位置 (出力長だが使わない)
              ]
              i32Type
          )
      ]

    inputE :: Int -> WasmExpr
    -- inputE _ = callExpr mod inputFunction [ constE 0 ] i32Type
    inputE offset = blockExpr mod
      [ storeExpr mod cell2 (constExpr mod 8) 0 (addE (getPointerE unit) (constE offset)) -- Pointer を保存
      , dropExpr mod (callExpr mod "fd_read" [ constE 0, constE 8, constE 1, constE 20 ] i32Type)
      , ifE
          (subE (loadExpr mod cell2 (constExpr mod 20) 0) (constE 1))
          (storeMemoryE offset (constE 0))
      ]

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

    compileRightValue :: RightValueT -> WasmExpr
    compileRightValue = case _ of
      FromMemoryT offset -> loadMemoryE offset
      ConstantT value -> constE value
      AddT left right -> addE (compileRightValue left) (compileRightValue right)
      SubT left right -> subE (compileRightValue left) (compileRightValue right)
      MulT left right -> mulE (compileRightValue left) (compileRightValue right)
      DivT left right -> divE (compileRightValue left) (compileRightValue right)
      FromMemoryTee diff offset -> loadMemoryTeeE (compileRightValue diff) offset

    go :: String -> IRT -> State Int (List WasmExpr)
    go label initIRT = flip tailRecM (initIRT /\ mempty) \(irt /\ acc) -> case irt of
      List.Nil -> pure $ Rec.Done acc
      st List.: ir' -> do
        h <- case st of
          AdditionT offset (MulT (ConstantT (-1)) right) -> pure $ storeMemoryE offset (subE (loadMemoryE offset) (compileRightValue right))
          AdditionTee diff offset (MulT (ConstantT (-1)) right) -> pure $ storeMemoryTeeE (compileRightValue diff) offset (subE (loadMemoryE offset) (compileRightValue right))
          AdditionT offset right -> pure $ storeMemoryE offset (addE (loadMemoryE offset) (compileRightValue right))
          AdditionTee diff offset right -> pure $ storeMemoryTeeE (compileRightValue diff) offset (addE (loadMemoryE offset) (compileRightValue right))
          AssignmentT offset right -> pure $ storeMemoryE offset (compileRightValue right)
          AssignmentTee diff offset right -> pure $ storeMemoryTeeE (compileRightValue diff) offset (compileRightValue right)
          LoopT _ body -> do
            label' <- mkLabel
            bodyE <- go label' body
            pure $ loopE label'
              ( ifE
                  (loadMemoryE 0)
                  (blockE (bodyE <> List.singleton (brE label')))
              )
          IfT cond tExp -> do
            tExp' <- go label tExp
            pure $ ifE (compileRightValue cond) (blockE tExp')
          OutputT right -> pure $ outputE (compileRightValue right)
          MovePointerT right -> pure $ movePointerE (compileRightValue right)
          InputT offset -> pure (inputE offset)
        -- t <- go label ir'
        -- pure $ h List.: t
        pure $ Rec.Loop (ir' /\ (acc <> List.singleton h))

  let exprs = evalState (go "label" (optTee $ irToIrT ir)) 0

  -- (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  addFunctionImport mod "fd_write" "wasi_snapshot_preview1" "fd_write" (createType [ i32Type, i32Type, i32Type, i32Type ]) i32Type
  -- (import "wasi_snapshot_preview1" "fd_read" (func $fd_read (param i32 i32 i32) (result i32)))
  addFunctionImport mod "fd_read" "wasi_snapshot_preview1" "fd_read" (createType [ i32Type, i32Type, i32Type, i32Type ]) i32Type

  addFunction mod "main"
    noneType
    noneType
    [ i32Type {- Pointer -} ]
    ( blockE
        ( List.fromFoldable
            [ localSet mod 0 (constE 1024)
            , storeExpr mod cell2 (constExpr mod 0) 0 (constExpr mod 16) -- output 用のポインタ
            , storeExpr mod cell2 (constExpr mod 4) 0 (constExpr mod 1) -- output の長さ 1 バイト
            , storeExpr mod cell2 (constExpr mod 8) 0 (constExpr mod 0) -- input 用のポインタ
            , storeExpr mod cell2 (constExpr mod 12) 0 (constExpr mod 1) -- input の長さ 1 バイト
            ] <> exprs <> List.singleton (returnExpr mod)
        )
    ) -- 1024 までずらしている 0 ~ 128 あたりは自由に使って OK！ (fd_write とかで使う)

  addFunctionExport mod "main" "_start"

  -- setStart mod "main"

  validate mod
  optimize mod

  emitBinary mod

module Brainfuck.Optimizer where

import Prelude

import Brainfuck.IR (IR, Offset, RightValue(..), Statement(..))
import Control.Apply (lift2)
import Control.Monad.State (State, evalState, get, modify_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List ((..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))

{----------
Composition
----------}

-- | RightValue が使用している Offset の集合
variables :: RightValue -> Set Offset
variables = case _ of
  FromMemory offset -> Set.singleton offset
  Constant _ -> Set.empty
  Add left right -> Set.union (variables left) (variables right)
  Sub left right -> Set.union (variables left) (variables right)
  Mul left right -> Set.union (variables left) (variables right)
  Div left right -> Set.union (variables left) (variables right)

-- | RightValue 内部の Variable の Offset 全てをずらす
addOffsetToVariables :: Offset -> RightValue -> RightValue
addOffsetToVariables offset = case _ of
  FromMemory offset' -> FromMemory (offset + offset')
  Constant value -> Constant value
  Add left right -> Add (addOffsetToVariables offset left) (addOffsetToVariables offset right)
  Sub left right -> Sub (addOffsetToVariables offset left) (addOffsetToVariables offset right)
  Mul left right -> Mul (addOffsetToVariables offset left) (addOffsetToVariables offset right)
  Div left right -> Div (addOffsetToVariables offset left) (addOffsetToVariables offset right)

-- | Statement 内部の Variable の Offset 全てをずらす
addOffsetToStatement :: Offset -> Statement -> Statement
addOffsetToStatement offset = case _ of
  Assignment offset' right -> Assignment (offset + offset') (addOffsetToVariables offset right)
  Addition offset' right -> Addition (offset + offset') (addOffsetToVariables offset right)
  Loop isOpt ir -> Loop isOpt (map (addOffsetToStatement offset) ir)
  If cond tExp -> If (addOffsetToVariables offset cond) (map (addOffsetToStatement offset) tExp)
  Output right -> Output (addOffsetToVariables offset right)
  MovePointer right -> MovePointer (addOffsetToVariables offset right)
  Input offset' -> Input (offset + offset')

-- | 1 番目の Statement を 2 番目と交換したとき、 それぞれどう更新されるか
-- | 交換不可能な場合は Nothing
switches :: Statement -> Statement -> Maybe (Statement /\ Statement)
switches st1 st2 = case st1 of
  Assignment offset1 right1 -> case st2 of
    Assignment offset2 right2
      | offset1 /= offset2
          && not (Set.member offset1 (variables right2))
          && not (Set.member offset2 (variables right1)) -> Just $ st2 /\ st1 -- それぞれ依存しているとダメ
    Addition offset2 right2
      | offset1 /= offset2
          && not (Set.member offset1 (variables right2))
          && not (Set.member offset2 (variables right1)) -> Just $ st2 /\ st1
    MovePointer (Constant p) -> Just $ st2 /\ addOffsetToStatement (-p) st1
    Output right2 | not (Set.member offset1 (variables right2)) -> Just $ st2 /\ st1
    _ -> Nothing
  Addition offset1 right1 -> case st2 of
    Assignment offset2 right2
      | offset1 /= offset2
          && not (Set.member offset1 (variables right2))
          && not (Set.member offset2 (variables right1)) -> Just $ st2 /\ st1
    Addition offset2 right2
      | offset1 /= offset2
          && not (Set.member offset1 (variables right2))
          && not (Set.member offset2 (variables right1)) -> Just $ st2 /\ st1
    MovePointer (Constant p) -> Just $ st2 /\ addOffsetToStatement (-p) st1
    Output right2 | not (Set.member offset1 (variables right2)) -> Just $ st2 /\ st1
    _ -> Nothing
  MovePointer (Constant p) -> case st2 of
    MovePointer _ -> Nothing -- MovePointer 同士は交換できないが、合成可能
    Loop _ _ -> Nothing -- Loop は常に Pointer 位置の参照をして分岐するため、入れ替えると死ぬ
    _ -> Just $ addOffsetToStatement p st2 /\ st1 -- それ以外の命令ないら適当に入れ替えて OK!
  Output right1 -> case st2 of
    Assignment offset2 _
      | not (Set.member offset2 (variables right1)) -> Just $ st2 /\ st1
    Addition offset2 _
      | not (Set.member offset2 (variables right1)) -> Just $ st2 /\ st1
    _ -> Nothing
  _ -> Nothing

-- | 投げられる値は addOpt を使って計算したものと仮定する
-- | すなわち一番左の Constant は必ず 1 重であり、左結合
addOpt :: RightValue -> RightValue -> RightValue
addOpt (Constant 0) right = right
addOpt left (Constant 0) = left
addOpt (Constant n) (Constant m) = Constant (n + m)
addOpt (Constant n) (Add (Constant m) right)
  | n + m == 0 = right
  | otherwise = Add (Constant (n + m)) right
addOpt (Add (Constant n) left) (Constant m)
  | n + m == 0 = left
  | otherwise = Add (Constant (n + m)) left
addOpt (Add (Constant n) left) (Add (Constant m) right)
  | n + m == 0 = Add left right
  | otherwise = Add (Constant (n + m)) (Add left right)
addOpt left right = Add left right

mulOpt :: RightValue -> RightValue -> RightValue
mulOpt (Constant 0) _ = Constant 0
mulOpt _ (Constant 0) = Constant 0
mulOpt (Constant 1) right = right
mulOpt left (Constant 1) = left
mulOpt (Constant n) (Constant m) = Constant (n * m)
mulOpt (Constant n) (Mul (Constant m) right)
  | n * m == 0 = Constant 0
  | n * m == 1 = right
  | otherwise = Mul (Constant (n * m)) right
mulOpt (Mul (Constant n) left) (Constant m)
  | n * m == 0 = Constant 0
  | n * m == 1 = left
  | otherwise = Mul (Constant (n * m)) left
mulOpt (Mul (Constant n) left) (Mul (Constant m) right)
  | n * m == 0 = Constant 0
  | n * m == 1 = Mul left right
  | otherwise = Mul (Constant (n * m)) (Mul left right)
mulOpt left right = Mul left right

-- | 2 つの Statement を合成
-- | 合成不能な場合は Nothing
composite :: Statement -> Statement -> Maybe Statement
composite st1 st2 = case st1 of
  Assignment offset1 right1 -> case st2 of
    Assignment offset2 right2 -> if offset1 == offset2 then Just $ Assignment offset1 right2 else Nothing
    Addition offset2 right2 -> if offset1 == offset2 then Just $ Assignment offset1 (addOpt right1 right2) else Nothing
    _ -> Nothing
  Addition offset1 right1 -> case st2 of
    Assignment offset2 right2 -> if offset1 == offset2 then Just $ Assignment offset1 right2 else Nothing
    Addition offset2 right2 -> if offset1 == offset2 then Just $ Addition offset1 (addOpt right1 right2) else Nothing
    _ -> Nothing
  MovePointer right1 -> case st2 of
    MovePointer right2 -> Just $ MovePointer (addOpt right1 right2)
    _ -> Nothing
  _ -> Nothing

-- | Move 命令をなるべく前に寄せる (こうすると tee 最適化が効きやすい！)
movePointerForward :: IR -> IR
movePointerForward ir = case ir of
  List.Nil -> List.Nil
  st : sts ->
    let
      forwarded = movePointerForward sts
    in
      case forwarded of
        MovePointer right : xs -> case switches st (MovePointer right) of
          Just (MovePointer right' /\ st') -> MovePointer right' : st' : xs
          _ -> st : MovePointer right : xs
        _ -> st : forwarded

-- | IR の最初に Statement を合成
consIR :: Statement -> IR -> IR
consIR st ir = case ir of
  List.Nil -> st : List.Nil
  x : xs -> case composite st x of
    Just st' -> st' : xs
    Nothing -> case switches st x of
      Just (x' /\ st') -> x' : (consIR st' xs)
      _ -> st : ir

{----
Loops
----}

-- | f が成り立つ命令が発生する可能性があるか
isMayHappen :: (Statement -> Boolean) -> IR -> Boolean
isMayHappen f ir = case ir of
  List.Nil -> false
  x : _ | f x -> true
  Loop _ ir' : xs -> isMayHappen f ir' || isMayHappen f xs
  If _ ir' : xs -> isMayHappen f ir' || isMayHappen f xs
  _ : xs -> isMayHappen f xs

-- | f が成り立つ命令が必ず一回のみ発生するときその命令を返す
-- | 一回も発生しない, または複数回発生する場合は Nothing
exactHappenOnce :: (Statement -> Boolean) -> IR -> Maybe Statement
exactHappenOnce f ir = case ir of
  List.Nil -> Nothing
  x : xs | f x && not (isMayHappen f xs) -> Just x
  x : _ | f x -> Nothing -- f x が複数回発生する可能性がある
  Loop _ ir' : xs | not (isMayHappen f ir') -> exactHappenOnce f xs
  If _ ir' : xs | not (isMayHappen f ir') -> exactHappenOnce f xs
  _ : xs -> exactHappenOnce f xs -- assert :  f x == false

-- | st を　 repeat 回実行した時の statement を返す
-- | 作れなければ Nothing
repeatStatement :: Statement -> RightValue -> Maybe Statement
repeatStatement st rv = case st of
  Assignment offset right -> Just $ If rv (List.fromFoldable [ Assignment offset right ])
  Addition offset right -> Just $ Addition offset (mulOpt right rv)
  _ -> Nothing

assignedOffsets :: IR -> Set Offset
assignedOffsets ir = case ir of
  List.Nil -> Set.empty
  Assignment offset _ : xs -> Set.insert offset (assignedOffsets xs)
  Addition offset _ : xs -> Set.insert offset (assignedOffsets xs)
  Loop _ ir' : xs -> Set.union (assignedOffsets ir') (assignedOffsets xs)
  If _ ir' : xs -> Set.union (assignedOffsets ir') (assignedOffsets xs)
  _ : xs -> assignedOffsets xs

variablesInIR :: IR -> Set Offset
variablesInIR ir = case ir of
  List.Nil -> Set.empty
  Assignment _ right : xs -> Set.union (variables right) (variablesInIR xs)
  Addition _ right : xs -> Set.union (variables right) (variablesInIR xs)
  Loop _ ir' : xs -> Set.union (variablesInIR ir') (variablesInIR xs)
  If cond ir' : xs -> Set.union (variables cond) (Set.union (variablesInIR ir') (variablesInIR xs))
  Output right : xs -> Set.union (variables right) (variablesInIR xs)
  MovePointer right : xs -> Set.union (variables right) (variablesInIR xs)
  Input _ : xs -> variablesInIR xs

noInternalReferences :: IR -> Boolean
noInternalReferences ir = Set.isEmpty (Set.intersection (assignedOffsets ir) (variablesInIR ir))

-- | Loop 命令を他の命令に最適化可能なときは変換
-- | 他の命令に変更できない場合も、optimizeOrphanIR を再起的に呼び body に適用する
optimizeLoop :: IR -> IR
optimizeLoop body =
  let
    optimizedBody = optimizeOrphanIR body

    -- Loop が最適化できるのは、(最適化後の)body が
    --  Loop, Input, Output, If, MovePointer を持たない (つまり Assignment, Addition のみ)
    --  それぞれの Assignment, Addition がそれぞれに依存していない (つまり、右辺で使われているいる値が、ループ内部で再代入されていない)
    --  位置 0 には一回のみ命令が発行されている
    -- のときのみ
    --　
    -- 現在の位置 0 の値を value とすると、
    -- body の 位置 0 の命令が
    -- Addition diff の場合、
    --  Div val (- diff) 回 内部の命令をくりかえす (なお、 val が diff の倍数でないとき動作が実際と異なるが、実際では必ず無限ループしてしまうので無視)
    --   diff が Constant (-1) のときは val 回に最適化する (val が Constant のときはさらなる最適化を期待できる)
    -- Assignment 0 rv の場合、
    --  rv が Constant 0 のときは、If 文に最適化できる
    --   body を、val が 0 のときは実行しない、val が 0 以外のときは実行する
    --  rv が Constant 0 以外のときは適当に実行
    --
    -- あとから気づいたけど if 分に最適化するときは内部で依存関係があっても大丈夫だな

    onlyAssignOrAddition = not
      ( isMayHappen
          case _ of
            Assignment _ _ -> false
            Addition _ _ -> false
            _ -> true
          optimizedBody
      )

    position0Statement = exactHappenOnce
      ( case _ of
          Assignment 0 _ -> true
          Addition 0 _ -> true
          _ -> false
      )
      optimizedBody
  in
    case position0Statement of
      Nothing -> List.fromFoldable [ Loop true optimizedBody ] -- 最適化できない
      Just st | onlyAssignOrAddition -> case st of
        Assignment 0 (Constant 0) ->
          List.fromFoldable [ If (FromMemory 0) optimizedBody ] --　If 文に最適化
        Assignment 0 _ -> List.fromFoldable [ Loop true optimizedBody ]
        Addition 0 (Constant (-1)) | noInternalReferences optimizedBody ->
          List.mapMaybe
            ( case _ of
                Addition 0 _ -> Nothing
                s -> repeatStatement s (FromMemory 0)
            )
            optimizedBody <> List.fromFoldable [ Assignment 0 (Constant 0) ]
        Addition 0 right | noInternalReferences optimizedBody ->
          List.mapMaybe
            ( case _ of
                Addition 0 _ -> Nothing
                s -> repeatStatement s (Div (FromMemory 0) (Sub (Constant 0) right))
            )
            optimizedBody <> List.fromFoldable [ Assignment 0 (Constant 0) ]
        _ -> List.fromFoldable [ Loop true optimizedBody ]
      _ -> List.fromFoldable [ Loop true optimizedBody ]

-- | すべての　 Loop 命令を最適化
-- | 再起的に optimizeOrphanIR を呼び出すことに注意
optimizeAllLoop :: IR -> IR
optimizeAllLoop ir = case ir of
  List.Nil -> List.Nil
  (Loop true ir') : xs -> optimizeLoop ir' <> optimizeAllLoop xs
  x : xs -> x : optimizeAllLoop xs

-- | 合成可能な部分を全て合成する
compositeAll :: IR -> IR
compositeAll ir = case ir of
  List.Nil -> List.Nil
  x : xs -> consIR x (compositeAll xs)

{-------
Ignoring
-------}

canIgnore :: Statement -> Boolean
canIgnore = case _ of
  Addition _ (Constant 0) -> true
  MovePointer (Constant 0) -> true
  _ -> false

ignoreAll :: IR -> IR
ignoreAll ir = case ir of
  List.Nil -> List.Nil
  x : xs -> if canIgnore x then ignoreAll xs else x : ignoreAll xs

{---------
Evaluating
---------}

substRightValue :: RightValue -> State (Map Offset Int) RightValue
substRightValue rv = get >>= \known -> case rv of
  FromMemory offset
    | Just value <- Map.lookup offset known -> pure $ Constant value
    | otherwise -> pure rv
  Add left right -> Add <$> substRightValue left <*> substRightValue right
  Sub left right -> Sub <$> substRightValue left <*> substRightValue right
  Mul left right -> Mul <$> substRightValue left <*> substRightValue right
  Div left right -> Div <$> substRightValue left <*> substRightValue right
  _ -> pure rv

simpRightValue :: RightValue -> RightValue
simpRightValue = case _ of
  Add (Constant 0) right -> right
  Add left (Constant 0) -> left
  Sub left (Constant 0) -> left
  Mul (Constant 0) _ -> Constant 0
  Mul _ (Constant 0) -> Constant 0
  Mul (Constant 1) right -> right
  Mul left (Constant 1) -> left
  Div left (Constant 1) -> left
  Add left (Mul (Constant (-1)) right) -> Sub left right
  right -> right

tryEval :: RightValue -> State (Map Offset Int) (Maybe Int)
tryEval = substRightValue >=> case _ of
  Constant value -> pure $ Just value
  Add left right -> do
    left' <- tryEval left
    right' <- tryEval right
    pure (lift2 (+) left' right')
  Sub left right -> do
    left' <- tryEval left
    right' <- tryEval right
    pure (lift2 (-) left' right')
  Mul left right -> do
    left' <- tryEval left
    right' <- tryEval right
    pure (lift2 (*) left' right')
  Div left right -> do
    left' <- tryEval left
    right' <- tryEval right
    pure (lift2 div left' right')
  _ -> pure Nothing

addKnown :: Offset -> Int -> State (Map Offset Int) Unit
addKnown offset value = modify_ (Map.insert offset value)

removeKnown :: Offset -> State (Map Offset Int) Unit
removeKnown offset = modify_ (Map.delete offset)

removeAllKnown :: State (Map Offset Int) Unit
removeAllKnown = modify_ (const Map.empty)

getKnown :: Offset -> State (Map Offset Int) (Maybe Int)
getKnown offset = Map.lookup offset <$> get

moveOffset :: Offset -> State (Map Offset Int) Unit
moveOffset offset = do
  let
    mapKeys :: forall k a. Ord k => (k -> k) -> Map k a -> Map k a
    mapKeys f m = Map.fromFoldable $ Map.values $ mapWithIndex (\k v -> f k /\ v) m
  modify_ (mapKeys (_ + offset))

-- 計算実行時に影響を受ける可能性のある

-- 計算を進めて　 [ ] を実際の値に変換する
-- ループの中は再起的に実行しない
eval :: IR -> State (Map Offset Int) IR
eval = case _ of
  Assignment offset right : xs -> do
    value <- tryEval right
    case value of
      Just value' -> do
        addKnown offset value'
        (Assignment offset (Constant value') : _) <$> eval xs
      Nothing -> do
        removeKnown offset
        sbst <- simpRightValue <$> substRightValue right
        (Assignment offset sbst : _) <$> eval xs
  Addition offset right : xs -> do
    value <- tryEval right
    case value of
      Just value' -> do
        known <- getKnown offset
        case known of
          Just known' -> do
            addKnown offset (known' + value')
            (Assignment offset (Constant (known' + value')) : _) <$> eval xs
          Nothing -> do
            removeKnown offset
            sbst <- simpRightValue <$> substRightValue right
            (Addition offset sbst : _) <$> eval xs
      Nothing -> do
        removeKnown offset
        sbst <- simpRightValue <$> substRightValue right
        (Addition offset sbst : _) <$> eval xs
  Loop isOpt ir : xs -> do
    removeAllKnown
    addKnown 0 0
    (Loop isOpt ir : _) <$> eval xs
  If cond ir : xs -> do
    cond' <- tryEval cond
    case cond' of
      Just 0 -> eval xs
      Just _ -> (ir <> _) <$> eval xs
      Nothing -> do
        removeAllKnown
        cond'' <- substRightValue cond
        (If cond'' ir : _) <$> eval xs
  Output right : xs -> do
    sbst <- simpRightValue <$> substRightValue right
    (Output sbst : _) <$> eval xs
  MovePointer right : xs -> do
    pm <- tryEval right
    case pm of
      Just p -> do
        moveOffset (-p)
        sbst <- simpRightValue <$> substRightValue right
        (MovePointer sbst : _) <$> eval xs
      Nothing -> do
        removeAllKnown
        sbst <- simpRightValue <$> substRightValue right
        (MovePointer sbst : _) <$> eval xs
  Input offset : xs -> do
    removeKnown offset
    (Input offset : _) <$> eval xs
  List.Nil -> pure List.Nil

-- | IR -> IR
optimizeOrphanIR :: IR -> IR
optimizeOrphanIR ir =
  optimizeAllLoop ir
    # compositeAll -- 合成可能な部分を合成する
    # ignoreAll -- 無視できる部分を無視する
    # (\ir' -> evalState (eval ir') Map.empty)
    # compositeAll -- 合成可能な部分を合成する
    # ignoreAll -- 無視できる部分を無視する
    # movePointerForward

optimize :: Int -> IR -> IR
optimize n ir =
  optimizeAllLoop ir
    # compositeAll -- 合成可能な部分を合成する
    # ignoreAll -- 無視できる部分を無視する
    # (\ir' -> evalState (eval ir') (Map.fromFoldable $ map (\i -> i /\ 0) $ (0 .. n)))
    # compositeAll -- 合成可能な部分を合成する
    # ignoreAll -- 無視できる部分を無視する
    # movePointerForward

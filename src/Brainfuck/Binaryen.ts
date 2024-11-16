"use strict";

import binaryen from "binaryen";

export const newModule = () => new binaryen.Module();

export const setMemory =
  (module: binaryen.Module) => (initial: number) => (maximum: number) => () =>
    module.setMemory(initial, maximum);

export const addMemoryExport =
  (module: binaryen.Module) => (name: string) => () =>
    module.addMemoryExport("0", name);

export const nopExpr = (module: binaryen.Module) => module.nop();

export const constExpr = (module: binaryen.Module) => (value: number) =>
  module.i32.const(value);

export const localGet = (module: binaryen.Module) => (index: number) =>
  module.local.get(index, binaryen.i32);

export const localSet =
  (module: binaryen.Module) =>
  (index: number) =>
  (value: binaryen.ExpressionRef) =>
    module.local.set(index, value);

export const localTee =
  (module: binaryen.Module) =>
  (index: number) =>
  (value: binaryen.ExpressionRef) =>
    module.local.tee(index, value, binaryen.i32);

export const addExpr =
  (module: binaryen.Module) =>
  (left: binaryen.ExpressionRef) =>
  (right: binaryen.ExpressionRef) =>
    module.i32.add(left, right);

export const subExpr =
  (module: binaryen.Module) =>
  (left: binaryen.ExpressionRef) =>
  (right: binaryen.ExpressionRef) =>
    module.i32.sub(left, right);

export const mulExpr =
  (module: binaryen.Module) =>
  (left: binaryen.ExpressionRef) =>
  (right: binaryen.ExpressionRef) =>
    module.i32.mul(left, right);

export const divExpr =
  (module: binaryen.Module) =>
  (left: binaryen.ExpressionRef) =>
  (right: binaryen.ExpressionRef) =>
    module.i32.div_s(left, right);

export const loadExpr =
  (module: binaryen.Module) =>
  (cellSize: 0 | 1 | 2) =>
  (ptr: binaryen.ExpressionRef) =>
  (offset: number) => {
    const align = 2 ** cellSize;
    const loadByAlign =
      cellSize === 0
        ? module.i32.load8_u
        : cellSize === 1
        ? module.i32.load16_u
        : module.i32.load;

    return loadByAlign(offset, align, ptr);
  };

export const storeExpr =
  (module: binaryen.Module) =>
  (cellSize: 0 | 1 | 2) =>
  (ptr: binaryen.ExpressionRef) =>
  (offset: number) =>
  (value: binaryen.ExpressionRef) => {
    const align = 2 ** cellSize;
    const storeByAlign =
      cellSize === 0
        ? module.i32.store8
        : cellSize === 1
        ? module.i32.store16
        : module.i32.store;

    return storeByAlign(offset, align, ptr, value);
  };

export const callExpr =
  (module: binaryen.Module) =>
  (name: string) =>
  (params: binaryen.ExpressionRef[]) =>
  (resultType: binaryen.Type) =>
    module.call(name, params, resultType);

export const dropExpr =
  (module: binaryen.Module) => (expr: binaryen.ExpressionRef) =>
    module.drop(expr);

export const i32Type = binaryen.i32;

export const noneType = binaryen.none;

export const createType = (params: [binaryen.Type]) =>
  binaryen.createType(params);

export const ifExpr =
  (module: binaryen.Module) =>
  (condition: binaryen.ExpressionRef) =>
  (ifTrue: binaryen.ExpressionRef) =>
    module.if(condition, ifTrue);

export const blockExpr =
  (module: binaryen.Module) => (children: binaryen.ExpressionRef[]) =>
    module.block("", children);

export const loopExpr =
  (module: binaryen.Module) =>
  (label: string) =>
  (body: binaryen.ExpressionRef) =>
    module.loop(label, body);

export const brExpr = (module: binaryen.Module) => (label: string) =>
  module.br(label);

export const returnExpr = (module: binaryen.Module) => module.return();

export const cell0 = 0 as const;
export const cell1 = 1 as const;
export const cell2 = 2 as const;

export const getAlign = (cellSize: 0 | 1 | 2) => 2 ** cellSize;

// IO

export const addFunctionImport =
  (module: binaryen.Module) =>
  (internalName: string) =>
  (externalModuleName: string) =>
  (externalBaseName: string) =>
  (params: binaryen.Type) =>
  (results: binaryen.Type) =>
  () =>
    module.addFunctionImport(
      internalName,
      externalModuleName,
      externalBaseName,
      params,
      results
    );

export const addFunctionExport =
  (module: binaryen.Module) =>
  (internalName: string) =>
  (externalName: string) =>
  () =>
    module.addFunctionExport(internalName, externalName);

export const addFunction =
  (module: binaryen.Module) =>
  (name: string) =>
  (params: binaryen.Type) =>
  (results: binaryen.Type) =>
  (locals: binaryen.Type[]) =>
  (body: binaryen.ExpressionRef) =>
  () =>
    module.addFunction(name, params, results, locals, body);

export const setStart = (module: binaryen.Module) => (name: string) => () =>
  module.setStart(module.getFunction(name));

// Optimizations & Compilation

export const optimize = (module: binaryen.Module) => () => module.optimize();

export const optimizeFunction =
  (module: binaryen.Module) => (name: string) => () =>
    module.optimizeFunction(name);

export const validate = (module: binaryen.Module) => () => module.validate();

export const emitBinary = (module: binaryen.Module) => () =>
  module.emitBinary();

export const binaryToBuffer = (binary: Uint8Array) => () =>
  Buffer.from(binary.buffer);

export const emitText = (module: binaryen.Module) => () => module.emitText();

export const setOptimizeLevel =
  (module: binaryen.Module) => (level: number) => () =>
    setOptimizeLevel(module)(level);

export const setShrinkLevel =
  (module: binaryen.Module) => (level: number) => () =>
    setShrinkLevel(module)(level);

export const emitStackIR = (module: binaryen.Module) => () =>
  module.emitStackIR();

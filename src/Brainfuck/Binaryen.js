"use strict";
import binaryen from "binaryen";
export const newModule = () => new binaryen.Module();
export const setMemory = (module) => (initial) => (maximum) => () => module.setMemory(initial, maximum);
export const addMemoryExport = (module) => (name) => () => module.addMemoryExport("0", name);
export const nopExpr = (module) => module.nop();
export const constExpr = (module) => (value) => module.i32.const(value);
export const localGet = (module) => (index) => module.local.get(index, binaryen.i32);
export const localSet = (module) => (index) => (value) => module.local.set(index, value);
export const addExpr = (module) => (left) => (right) => module.i32.add(left, right);
export const subExpr = (module) => (left) => (right) => module.i32.sub(left, right);
export const mulExpr = (module) => (left) => (right) => module.i32.mul(left, right);
export const divExpr = (module) => (left) => (right) => module.i32.div_s(left, right);
export const loadExpr = (module) => (cellSize) => (ptr) => (offset) => {
    const align = 2 ** cellSize;
    const loadByAlign = cellSize === 0
        ? module.i32.load8_u
        : cellSize === 1
            ? module.i32.load16_u
            : module.i32.load;
    return loadByAlign(offset, align, ptr);
};
export const storeExpr = (module) => (cellSize) => (ptr) => (offset) => (value) => {
    const align = 2 ** cellSize;
    const storeByAlign = cellSize === 0
        ? module.i32.store8
        : cellSize === 1
            ? module.i32.store16
            : module.i32.store;
    return storeByAlign(offset, align, ptr, value);
};
export const callExpr = (module) => (name) => (params) => (resultType) => module.call(name, params, resultType);
export const i32Type = binaryen.i32;
export const noneType = binaryen.none;
export const ifExpr = (module) => (condition) => (ifTrue) => module.if(condition, ifTrue);
export const blockExpr = (module) => (children) => module.block("", children);
export const loopExpr = (module) => (label) => (body) => module.loop(label, body);
export const brExpr = (module) => (label) => module.br(label);
export const returnExpr = (module) => module.return();
export const cell0 = 0;
export const cell1 = 1;
export const cell2 = 2;
export const getAlign = (cellSize) => 2 ** cellSize;
// IO
export const addFunctionImport = (module) => (internalName) => (externalModuleName) => (externalBaseName) => (params) => (results) => () => module.addFunctionImport(internalName, externalModuleName, externalBaseName, params, results);
export const addFunctionExport = (module) => (internalName) => (externalName) => () => module.addFunctionExport(internalName, externalName);
export const addFunction = (module) => (name) => (params) => (results) => (locals) => (body) => () => module.addFunction(name, params, results, locals, body);
// Optimizations & Compilation
export const optimize = (module) => () => module.optimize();
export const optimizeFunction = (module) => (name) => () => module.optimizeFunction(name);
export const validate = (module) => () => module.validate();
export const emitBinary = (module) => () => module.emitBinary();
export const emitText = (module) => () => module.emitText();
export const setOptimizeLevel = (module) => (level) => () => setOptimizeLevel(module)(level);
export const setShrinkLevel = (module) => (level) => () => setShrinkLevel(module)(level);
export const emitStackIR = (module) => () => module.emitStackIR();

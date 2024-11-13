"use strict";

export const newInstance =
  (moduleName) =>
  (inputName) =>
  (outputName) =>
  (input) =>
  (output) =>
  (wasmBinary) =>
  () =>
    new WebAssembly.Instance(new WebAssembly.Module(wasmBinary), {
      [`${moduleName}`]: {
        [`${inputName}`]: input,
        [`${outputName}`]: (str) => output(str)(),
      },
    });

export const runInstance = (instance) => (mainName) => () =>
  instance.exports[`${mainName}`]();

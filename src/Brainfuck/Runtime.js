"use strict";
const { WASI } = require("node:wasi");
const { argv, env } = require("node:process");

const wasi = new WASI({
  version: "preview1",
  args: argv,
  env,
});

export const newInstance =
  (wasmBinary) =>
  async () => {
    // wasi.start(new WebAssembly.Instance(new WebAssembly.Module(wasmBinary),importObj));
    const module = await WebAssembly.compile(wasmBinary);
    const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
    wasi.start(instance);
  };

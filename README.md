# purescript-brainfuck

```
npm ci
npm run bundle
node index.js "./examples/hello.bf"
```

## Implementation

Raw Code -Parser-> AST -Transpiler-> IR -Compiler-> WASM Binary => Runtime(wasmer)

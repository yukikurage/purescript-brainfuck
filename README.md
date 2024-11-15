# purescript-brainfuck

```
npm ci
npm run bundle
node index.js "./examples/hello.bf"
```

## Implementation

Raw Code -Parser-> AST -Transpiler-> IR -Compiler-> WASM Binary => Runtime(wasmer)

## Examples

bfbf.bf

```
node index.js ./examples/bfbf.bf "++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++++.>.+++.
------.--------.<<+.<.!"
```

Examples are collected via

- https://copy.sh/brainfuck/
- https://github.com/rdebath/Brainfuck/tree/master/testing

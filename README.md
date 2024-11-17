# purescript-brainfuck

```
npm ci
npm run bundle
node index.js "./examples/hello.bf"
```

(Use Wasmer)

```
wasmer create-exe examples/mandelbrot.wasm  -o ./examples/mandelbrot --use-wasmer-release https://github.com/wasmerio/wasmer/relea
ses/download/v5.0.1/wasmer-linux-amd64.tar.gz
```

## Implementation

Raw Code -Parser-> AST -Transpiler-> IR -Compiler-> WASM Binary => Runtime(wasmer)

## Examples

bfbf.bf

```
echo "++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++++.>.+++.
------.--------.<<+.<.!" | node index.js ./examples/bfbf.bf
```

```
cat examples/bfbf-fib.bf | node index.js ./examples/bfbf.bf
```

Examples are collected via

- https://copy.sh/brainfuck/
- https://github.com/rdebath/Brainfuck/tree/master/testing

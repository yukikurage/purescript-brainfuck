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

## Speed Comparison

(Mean of 4 runs)

| Example       | Original             | Binaryen-O2          | purescript-brainfuck    | Binaryen + purscript-brainfuck | (Wasmer create-exe)  |
| ------------- | -------------------- | -------------------- | ----------------------- | ------------------------------ | -------------------- |
| mandelbrot.bf | 3005ms (2981~3036ms) | 1679ms (1667~1692ms) | 1399ms (1394~1407ms)    | 1410ms (1379~1449ms)           | 1072ms (1043~1107ms) |
| hanoi.bf      | 7382ms (7368~7400ms) | 7404ms (7340~7452ms) | 100.0ms (91.00~113.0ms) | 90.25ms (81.00~94.00ms)        | 1274ms (1258~1294ms) |

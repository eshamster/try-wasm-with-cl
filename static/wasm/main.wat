;; should compile by "wat2wasm main.wat"
(module
  (import "console" "log" (func $log (param i32)))
  (func $hoge-ab
    i32.const 444
    call $log)
  (export "exported_func" (func $hoge-ab)))
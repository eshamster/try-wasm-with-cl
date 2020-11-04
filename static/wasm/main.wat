;; should compile by "wat2wasm main.wat"
(module
  (import "console" "log" (func $log (param i32)))
  (func (export "exported_func")
    i32.const 422
    call $log))
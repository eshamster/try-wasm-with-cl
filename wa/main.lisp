(defpackage :try-wasm-with-cl/wa/main
  (:use #:try-wasm-with-cl/wa/built-in-func
        #:try-wasm-with-cl/wa/defmacro
        #:try-wasm-with-cl/wa/defun
        #:try-wasm-with-cl/wa/export
        #:try-wasm-with-cl/wa/import
        #:try-wasm-with-cl/wa/module
        #:try-wasm-with-cl/wa/reserved-word
        #:try-wasm-with-cl/wa/default-macro)
  (:export #:defmacro.wat
           #:defun.wat
           #:defexport.wat
           #:defimport.wat
           #:generate-wat-module

           #:local
           #:block
           #:loop

           ;; built-in functions
           #:i32.add
           #:i32.const
           #:i32.eqz
           #:i32.ge-u

           #:get-local
           #:set-local

           #:br
           #:br-if))

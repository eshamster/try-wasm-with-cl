(defpackage :try-wasm-with-cl/wa/main
  (:use #:try-wasm-with-cl/wa/built-in-func
        #:try-wasm-with-cl/wa/defglobal
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
           #:defglobal.wat
           #:generate-wat-module

           #:func
           #:memory
           #:mut
           #:local
           #:block
           #:loop

           ;; default macros
           #:for
           #:i32+

           ;; built-in functions
           #:i32.add
           #:i32.sub
           #:i32.mul
           #:i32.const
           #:i32.eq
           #:i32.eqz
           #:i32.ge-u
           #:i32.store
           #:i32.load

           #:get-local
           #:set-local
           #:get-global
           #:set-global

           #:br
           #:br-if))

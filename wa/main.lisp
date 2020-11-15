(defpackage :try-wasm-with-cl/wa/main
  (:use :try-wasm-with-cl/wa/defmacro
        :try-wasm-with-cl/wa/defun
        :try-wasm-with-cl/wa/export
        :try-wasm-with-cl/wa/import
        :try-wasm-with-cl/wa/module)
  (:export :defmacro.wat
           :defun.wat
           :defexport.wat
           :defimport.wat
           :generate-wat-module))

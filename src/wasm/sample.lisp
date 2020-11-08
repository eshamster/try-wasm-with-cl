(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use :cl)
  (:import-from :try-wasm-with-cl/wa/main
                :defun.wat
                :defimport.wat
                :defexport.wat))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat console.log (func log ((i32))))

(defun.wat test-print () ()
  |i32.const| 5555
  |call| log)

(defexport.wat exported-func (func test-print))

(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use :cl)
  (:import-from :try-wasm-with-cl/wa/main
                :defun.wat
                :defimport.wat
                :defexport.wat))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat console.log (func log ((i32))))

(defun.wat sample ((x i32)) (i32)
  |get_local| x
  |i32.const| 100
  |i32.add|)

(defun.wat test-print () ()
  |i32.const| 300
  |call| sample
  |call| log)

(defexport.wat exported-func (func test-print))

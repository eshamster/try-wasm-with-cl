(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use :cl)
  (:import-from :try-wasm-with-cl/wa/main
                :defmacro.wat
                :defun.wat
                :defimport.wat
                :defexport.wat))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat log console.log (func ((i32))))

(defmacro.wat local-i32-const (x y)
  `(progn |get_local| ,x
          |i32.const| ,y))

(defun.wat sample ((x i32)) (i32)
  (local-i32-const x 100)
  |i32.add|)

(defun.wat test-print () ()
  |i32.const| 300
  |call| sample
  |call| log)

(defexport.wat exported-func (func test-print))

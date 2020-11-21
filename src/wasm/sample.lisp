(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use :cl)
  (:import-from :try-wasm-with-cl/wa/main
                :defmacro.wat
                :defun.wat
                :defimport.wat
                :defexport.wat

                :local))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat log console.log (func ((i32))))

(defmacro.wat local-i32-const (x y)
  `(progn |get_local| ,x
          |get_local| ,y))

(defun.wat sample ((x i32)) (i32)
  (local tmp i32)
  |i32.const| 100
  |set_local| tmp
  (local-i32-const x tmp)
  |i32.add|)

(defun.wat test-if ((x i32)) ()
  (if (|i32.eqz| (|get_local| x))
      (progn |i32.const| 10
             |call| log)
      (progn |i32.const| 20
             |call| log)))

(defun.wat test-print () ()
  |i32.const| 300
  |call| sample
  |call| log
  ;; test-if
  |i32.const| 0
  |call| test-if
  |i32.const| 1
  |call| test-if)

(defexport.wat exported-func (func test-print))

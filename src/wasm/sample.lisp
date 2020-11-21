(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use :cl)
  (:import-from :try-wasm-with-cl/wa/main
                :defmacro.wat
                :defun.wat
                :defimport.wat
                :defexport.wat

                :local
                :block
                :loop)
  (:import-from :alexandria
                :symbolicate))
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

(defmacro.wat incf-i32 (place &optional (added '(|i32.const| 1)))
  `(progn |get_local| ,place
          ,@added
          |i32.add|
          (|set_local| ,place)))

(defmacro.wat let (var-forms &body body)
  ;; Ex. (let (((i i32) (|i32.const| 0))
  ;;           (j i32))
  ;;       ...)
  ;; Can use this only at head of function
  `(progn ,@(mapcar (lambda (var-form)
                      (let ((var-type (if (listp (car var-form))
                                          (car var-form)
                                          var-form)))
                        (destructuring-bind (var type) var-type
                          `(local ,var ,type))))
                    var-forms)
          ,(mapcan (lambda (var-form)
                     (when (listp (car var-form))
                       (let ((var-type (car var-form))
                             (init (cadr var-form)))
                         (destructuring-bind (var type) var-type
                           (declare (ignore type))
                           `(|set_local| ,var ,init)))))
                   var-forms)
          ,@body))

(defmacro.wat for (for-name params &body body)
  (let ((block-name (symbolicate for-name "-BLOCK"))
        (loop-name  (symbolicate for-name "-LOOP")))
    (destructuring-bind (&key init break mod) params
      `(progn ,init
              (block ,block-name
                (loop ,loop-name
                       (|br_if| ,block-name ,break)
                      ,@body
                      ,mod
                      (|br| ,loop-name)))))))

(defun.wat test-for () ()
  (let ((i i32)
        ((max i32) (|i32.const| 5)))
    (for f (:init (|set_local| i (|i32.const| 1))
            :break (|i32.ge_u| (|get_local| i) (|get_local| max))
            :mod (incf-i32 i))
      |get_local| i
      |call| log)))

(defun.wat test-print () ()
  |i32.const| 300
  |call| sample
  |call| log
  ;; test-if
  |i32.const| 0
  |call| test-if
  |i32.const| 1
  |call| test-if
  ;; test-for
  |call| test-for)

(defexport.wat exported-func (func test-print))

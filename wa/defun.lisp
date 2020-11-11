(defpackage :try-wasm-with-cl/wa/defun
  (:use :cl
        :try-wasm-with-cl/wa/reserved-word)
  (:export :defun.wat
           :get-func-body-generators)
  (:import-from :try-wasm-with-cl/wa/import
                :get-imported-names)
  (:import-from :try-wasm-with-cl/wa/type
                :convert-type
                :parse-typeuse)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name
                :clone-list-with-modification)
  (:import-from :alexandria
                :hash-table-keys
                :hash-table-values
                :make-keyword
                :symbolicate))
(in-package :try-wasm-with-cl/wa/defun)

(defvar *funcs* (make-hash-table))

(defun get-func-names ()
  (hash-table-keys *funcs*))

(defun get-func-body-generators ()
  (hash-table-values *funcs*))

(defmacro defun.wat (name args result &body body)
  `(progn (setf (gethash ',name *funcs*)
                (lambda ()
                  (generate-defun ',name ',args ',result ',body)))))

(defun generate-defun (name args result body)
  (multiple-value-bind (parsed-typeuse vars)
      (parse-typeuse (list args result))
    `(|func|
      ,(parse-arg-name name)
      ,@parsed-typeuse
      ,@(parse-body body vars))))

(defun parse-body (body arg-names)
  (let ((vars (append arg-names
                      (get-func-names)
                      (get-imported-names))))
    (clone-list-with-modification
     body
     (lambda (sym)
       (if (find sym vars)
           (parse-arg-name sym)
           sym)))))

(defpackage :try-wasm-with-cl/wa/defun
  (:use :cl
        :try-wasm-with-cl/wa/reserved-word)
  (:export :defun.wat
           :get-func-bodies)
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

(defun get-func-bodies ()
  (hash-table-values *funcs*))

;; TODO: convert each symbol: abc-def -> abcDef
;;       (probably it should be done at printing)
(defmacro defun.wat (name args result &body body)
  ;; TODO: process "result"
  (multiple-value-bind (parsed-typeuse vars)
      (parse-typeuse (list args result))
    `(progn (setf (gethash ',name *funcs*)
                  '(|func|
                    ,(parse-arg-name name)
                    ,@parsed-typeuse
                    ,@(parse-body body vars))))))

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

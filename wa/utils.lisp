(defpackage :try-wasm-with-cl/wa/utils
  (:use :cl)
  (:export :clone-list-with-modification
           :parse-arg-name
           :symbol-to-string)
  (:import-from :alexandria
                :symbolicate)
  (:import-from :cl-ppcre
                :regex-replace-all))
(in-package :try-wasm-with-cl/wa/utils)

(defun clone-list-with-modification (list fn-each-sym)
  (labels ((rec (rest res)
             (if (atom rest)
                 (if res
                     (cons (funcall fn-each-sym rest) res)
                     (funcall fn-each-sym rest))
                 (mapcar (lambda (unit)
                           (rec unit res))
                         rest))))
    (rec list nil)))

(defun parse-arg-name (arg-name)
  (symbolicate '$ arg-name))

(defun symbol-to-string (sym)
  (string-downcase
   (regex-replace-all "-" (symbol-name sym) "_")))

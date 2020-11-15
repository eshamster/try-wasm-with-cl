(defpackage :try-wasm-with-cl/wa/body-parser
  (:use :cl)
  (:export :parse-body)
  (:import-from :try-wasm-with-cl/wa/environment
                :wenv-function-symbols
                :wenv-import-symbols)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name))
(in-package :try-wasm-with-cl/wa/body-parser)

(defun parse-body (body args)
  (let* ((vars (append args
                       (wenv-function-symbols)
                       (wenv-import-symbols)))
         (var-table (vars-to-table vars)))
    (clone-list-with-modification
     body
     (lambda (atom)
       (parse-atom atom var-table)))))

(defun vars-to-table (vars)
  (let ((res (make-hash-table)))
    (dolist (var vars)
      (setf (gethash var res) t))
    res))

(defun parse-atom (atom var-table)
  (if (gethash atom var-table)
      (parse-arg-name atom)
      atom))

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

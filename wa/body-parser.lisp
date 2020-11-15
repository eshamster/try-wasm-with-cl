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
                       (wenv-import-symbols))))
    (parse-body% body vars nil)))

(defun parse-body% (body vars res)
  (cond ((atom body)
         (parse-atom% body vars))
        (t (mapcar (lambda (unit)
                     (parse-body% unit vars res))
                   body))))

(defun parse-atom% (atom vars)
  (if (find atom vars)
      (parse-arg-name atom)
      atom))

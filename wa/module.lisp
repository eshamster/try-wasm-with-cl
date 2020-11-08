(defpackage :try-wasm-with-cl/wa/module
  (:use :cl)
  (:export :generate-wat-module)
  (:import-from :try-wasm-with-cl/wa/defun
                :get-func-bodies)
  (:import-from :try-wasm-with-cl/wa/export
                :get-export-bodies)
  (:import-from :try-wasm-with-cl/wa/import
                :get-import-bodies)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :|module|)
  (:import-from :try-wasm-with-cl/wa/utils
                :clone-list-with-modification))
(in-package :try-wasm-with-cl/wa/module)

(defun generate-wat-module% ()
  `(|module|
    ,@(get-import-bodies)
    ,@(get-func-bodies)
    ,@(get-export-bodies)))

(defun generate-wat-module ()
  (let ((str-list (clone-list-with-modification
                   (generate-wat-module%)
                   (lambda (elem)
                     (typecase elem
                       (symbol (symbol-name elem))
                       (string (format nil "~S" elem))
                       (t elem))))))
    str-list))


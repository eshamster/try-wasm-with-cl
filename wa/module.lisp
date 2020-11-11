(defpackage :try-wasm-with-cl/wa/module
  (:use :cl)
  (:export :generate-wat-module)
  (:import-from :try-wasm-with-cl/wa/defun
                :get-func-body-generators)
  (:import-from :try-wasm-with-cl/wa/export
                :get-export-body-generators)
  (:import-from :try-wasm-with-cl/wa/import
                :get-import-body-generators)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :|module|)
  (:import-from :try-wasm-with-cl/wa/utils
                :clone-list-with-modification))
(in-package :try-wasm-with-cl/wa/module)

(defun generate-wat-module% ()
  `(|module|
    ,@(mapcar #'funcall (get-import-body-generators))
    ,@(mapcar #'funcall (get-func-body-generators))
    ,@(mapcar #'funcall (get-export-body-generators))))

(defun generate-wat-module ()
  (let ((str-list (clone-list-with-modification
                   (generate-wat-module%)
                   (lambda (elem)
                     (typecase elem
                       (symbol (symbol-name elem))
                       (string (format nil "~S" elem))
                       (t elem))))))
    str-list))


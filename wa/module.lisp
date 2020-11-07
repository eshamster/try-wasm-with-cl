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
                :|module|))
(in-package :try-wasm-with-cl/wa/module)

(defun generate-wat-module ()
  `(|module|
    ,@(get-import-bodies)
    ,@(get-func-bodies)
    ,@(get-export-bodies)))

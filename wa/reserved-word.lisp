(defpackage :try-wasm-with-cl/wa/reserved-word
  (:use :cl)
  (:export :|module|
           :module-keyword-p

           :|import|
           :import-keyword-p
           :|export|
           :export-keyword-p
           :|func|
           :func-keyword-p
           :|param|
           :param-keyword-p
           :|result|
           :result-keyword-p

           :|i32|
           :i32-keyword-p)
  (:import-from :alexandria
                :symbolicate))
(in-package :try-wasm-with-cl/wa/reserved-word)

(defmacro defrw (sym)
  (let ((lower-sym (intern (string-downcase (symbol-name sym)))))
    `(progn (defvar ,lower-sym nil)
            (defun ,(symbolicate sym "-KEYWORD-P") (sym)
              (string= (symbol-name sym)
                       ,(symbol-name sym))))))

(defrw module)

(defrw import)
(defrw export)
(defrw func)
(defrw param)
(defrw result)

(defrw i32)
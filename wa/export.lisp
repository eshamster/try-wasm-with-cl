(defpackage :try-wasm-with-cl/wa/export
  (:use :cl)
  (:export :defexport.wat
           :get-export-bodies)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :|export|
                :|func|
                :func-keyword-p)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name
                :symbol-to-string)
  (:import-from :alexandria
                :hash-table-values))
(in-package :try-wasm-with-cl/wa/export)

;; https://webassembly.github.io/spec/core/text/modules.html#exports

(defvar *exports* (make-hash-table))

(defun get-export-bodies ()
  (hash-table-values *exports*))

(defmacro defexport.wat (js-func-name export-desc)
  ;; Ex. (defexport.wat js-func-name (func foo))
  ;;     -> (export "js_func_name" (func $foo))
  `(setf (gethash ,js-func-name *imports*)
         '(|export|
           ,(symbol-to-string js-func-name)
           ,(parse-export-desc export-desc))))

(defun parse-export-desc (export-desc)
  (let ((type  (car  export-desc))
        (param (cadr export-desc)))
    (cond ((func-keyword-p type)
           (parse-export-func-desc param))
          (t (error "not recognized export type: ~A" type)))))

(defun parse-export-func-desc (param)
  `(|func| ,(parse-arg-name param)))

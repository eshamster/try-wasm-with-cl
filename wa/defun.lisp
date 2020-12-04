(defpackage :try-wasm-with-cl/wa/defun
  (:use :cl
        :try-wasm-with-cl/wa/reserved-word)
  (:export :defun.wat)
  (:import-from :try-wasm-with-cl/wa/body-parser
                :parse-body)
  (:import-from :try-wasm-with-cl/wa/environment
                :wsymbol-function
                :intern.wat
                :wenv-function-symbols
                :wenv-import-symbols)
  (:import-from :try-wasm-with-cl/wa/type
                :convert-type
                :parse-typeuse)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name)
  (:import-from :alexandria
                :symbolicate))
(in-package :try-wasm-with-cl/wa/defun)

(defmacro defun.wat (name args result &body body)
  `(progn (setf (wsymbol-function (intern.wat ',name))
                (lambda ()
                  (generate-defun ',name ',args ',result ',body)))
          ,(defun-empty% name args)))

(defun generate-defun (name args result body)
  (multiple-value-bind (parsed-typeuse vars)
      (parse-typeuse (list args result))
    `(|func|
      ,(parse-arg-name name)
      ,@parsed-typeuse
      ,@(parse-body body vars))))

(defun defun-empty% (name args)
  (let ((args-var (mapcar #'car args)))
    `(defun ,name ,args-var
       (declare (ignore ,@args-var)))))

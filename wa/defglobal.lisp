(defpackage :try-wasm-with-cl/wa/defglobal
  (:use #:cl)
  (:export #:defglobal.wat)
  (:import-from #:try-wasm-with-cl/wa/environment
                #:wsymbol-import
                #:intern.wat)
  (:import-from #:try-wasm-with-cl/wa/reserved-word
                #:|global|
                #:global
                #:|mut|
                #:mut)
  (:import-from #:try-wasm-with-cl/wa/type
                #:convert-type)
  (:import-from #:try-wasm-with-cl/wa/utils
                #:parse-arg-name
                #:parse-mod-nm))
(in-package :try-wasm-with-cl/wa/defglobal)

(defmacro defglobal.wat (name mod-nm globaltype)
  ;; Ex. (defglobal.wat g js.global (mut i32))
  ;;     -> (global $g (import "js" "global") (mut i32))
  `(progn (setf (wsymbol-import (intern.wat ',name))
                (lambda ()
                  (generate-global-body
                   ',name ',mod-nm ',globaltype)))))

(defun generate-global-body (name mod-nm globaltype)
  `(|global|
    ,(parse-arg-name name)
    (|import| ,@(parse-mod-nm mod-nm))
    ,(parse-globaltype globaltype)))

(defun parse-globaltype (globaltype)
  (cond ((and (listp globaltype)
              (eq (car globaltype) 'mut))
         `(|mut| ,(convert-type (cadr globaltype))))
        (t (error "not implemented globaltype form: ~A" globaltype))))

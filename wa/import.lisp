(defpackage :try-wasm-with-cl/wa/import
  (:use #:cl)
  (:export #:defimport.wat)
  (:import-from #:try-wasm-with-cl/wa/environment
                #:wsymbol-import
                #:intern.wat)
  (:import-from #:try-wasm-with-cl/wa/reserved-word
                #:|import|
                #:|func|
                #:func
                #:|memory|
                #:memory)
  (:import-from #:try-wasm-with-cl/wa/type
                #:parse-typeuse)
  (:import-from #:try-wasm-with-cl/wa/utils
                #:parse-arg-name)
  (:import-from #:cl-ppcre
                #:split))
(in-package :try-wasm-with-cl/wa/import)

;; https://webassembly.github.io/spec/core/text/modules.html#imports

(defmacro defimport.wat (name mod-nm import-desc)
  ;; Ex. (defimport.wat lg console.log (func ((i32))))
  ;;     -> (import "console" "log" (func $lg (param i32)))
  `(progn (setf (wsymbol-import (intern.wat ',name))
                (lambda ()
                  (generate-import-body
                   ',name ',mod-nm ',import-desc)))))

(defun generate-import-body (name mod-nm parsed-import-desc)
  `(|import|
    ,@(parse-mod-nm mod-nm)
    ,(parse-import-desc name parsed-import-desc)))

(defun parse-mod-nm (mod-nm)
  (let* ((*print-case* :downcase)
         (mod-nm-str (format nil "~A" mod-nm))
         (splitted (split "\\." mod-nm-str)))
    (unless (= (length splitted) 2)
      (error "mod-nm should be \"xxx.yyy\" but got ~A" mod-nm))
    splitted))

(defun parse-import-desc (name import-desc)
  ;; TODO: should process 'table', 'global'
  (let ((keyword (car import-desc))
        (params (cdr import-desc)))
    (ecase keyword
      (func (parse-import-func-desc name params))
      (memory (parse-import-memory-desc name params)))))

(defun parse-import-func-desc (name params)
  ;; Ex. name: foo, params: (((a i32) (b i32)) (i32))
  ;;     -> (func $foo (param $a i32) (param $b i32) (result $i32))
  `(|func| ,(parse-arg-name name)
           ,@(parse-typeuse params)))

(defun parse-import-memory-desc (name params)
  ;; Ex. name: foo, params: (1)
  ;;     -> (memory $foo 1)
  `(|memory| ,(parse-arg-name name)
             ,(car params)))

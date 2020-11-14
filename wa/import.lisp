(defpackage :try-wasm-with-cl/wa/import
  (:use :cl)
  (:export :defimport.wat)
  (:import-from :try-wasm-with-cl/wa/environment
                :wsymbol-import
                :intern.wat)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :|import|
                :|func|
                :func-keyword-p)
  (:import-from :try-wasm-with-cl/wa/type
                :parse-typeuse)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name)
  (:import-from :cl-ppcre
                :split))
(in-package :try-wasm-with-cl/wa/import)

;; https://webassembly.github.io/spec/core/text/modules.html#imports

(defmacro defimport.wat (mod-nm import-desc)
  ;; Ex. (defimport.wat console.log (func log ((i32))))
  ;;     -> (import "console" "log" (func $log (param i32)))
  (multiple-value-bind (parsed-import-desc wat-name)
      (parse-import-desc import-desc)
    `(progn (setf (wsymbol-import (intern.wat ',wat-name))
                  (lambda ()
                    (generate-import-body
                     ',mod-nm ',parsed-import-desc))))))

(defun generate-import-body (mod-nm parsed-import-desc)
  `(|import|
    ,@(parse-mod-nm mod-nm)
    ,parsed-import-desc))

(defun parse-mod-nm (mod-nm)
  (let* ((*print-case* :downcase)
         (mod-nm-str (format nil "~A" mod-nm))
         (splitted (split "\\." mod-nm-str)))
    (unless (= (length splitted) 2)
      (error "mod-nm should be \"xxx.yyy\" but got ~A" mod-nm))
    splitted))

(defun parse-import-desc (import-desc)
  ;; TODO: should process 'table', 'memory', 'global'
  (let ((keyword (car import-desc))
        (params (cdr import-desc)))
    (cond ((func-keyword-p keyword)
           (parse-import-func-desc params))
          (t (error "not recognized keyword: ~A" keyword)))))

(defun parse-import-func-desc (params)
  ;; Ex. (foo ((a i32) (b i32)) (i32))
  ;;     -> (func $foo (param $a i32) (param $b i32) (result $i32)
  ;;        foo
  (if (atom (car params))
      (let ((id (car params)))
        (values `(|func| ,(parse-arg-name id)
                         ,@(parse-typeuse (cdr params)))
                id))
      (error "Don't allow case where id is not exist")))

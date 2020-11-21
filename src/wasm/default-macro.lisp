(defpackage :try-wasm-with-cl/wa/default-macro
  (:use :cl)
  (:import-from :try-wasm-with-cl/wa/defmacro
                :defmacro.wat)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :|if|
                :|then|
                :then
                :|else|
                :else))
(in-package :try-wasm-with-cl/wa/default-macro)

(defmacro.wat if (test-form then-form &optional else-form)
  `(|if| ,test-form
         ,@(if else-form
               `((|then| ,then-form)
                 (|else| ,else-form))
               `((|then| ,then-form)))))

(defmacro.wat when (test-form &body form)
  `(if ,test-form
       (progn ,@form)))

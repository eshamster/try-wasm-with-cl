(defpackage :try-wasm-with-cl/compiler
  (:use :cl)
  (:export :wat2wasm
           :ps2js)
  (:import-from :try-wasm-with-cl/src/js/main
                :main)
  (:import-from :ps-experiment
                :with-use-ps-pack)
  (:import-from :uiop
                :run-program))
(in-package :try-wasm-with-cl/compiler)

(defun wat2wasm (wat-path wasm-path)
  (run-program (format nil "wat2wasm ~S -o ~S"
                       (namestring wat-path)
                       (namestring wasm-path))))

(defun ps2js (js-path)
  (with-open-file (out js-path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ (with-use-ps-pack (:this)
             (main))
           out)))


(defpackage :try-wasm-with-cl/compiler
  (:use :cl)
  (:export :wat2wasm)
  (:import-from :uiop
                :run-program))
(in-package :try-wasm-with-cl/compiler)

(defun wat2wasm (wat-path wasm-path)
  (run-program (format nil "wat2wasm ~S -o ~S"
                       (namestring wat-path)
                       (namestring wasm-path))))

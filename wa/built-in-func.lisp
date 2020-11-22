(defpackage :try-wasm-with-cl/wa/built-in-func
  (:use #:cl)
  (:export #:built-in-func-p
           #:convert-built-in-func

           #:i32.add
           #:i32.const
           #:i32.eqz
           #:i32.ge-u

           #:get-local
           #:set-local

           #:br
           #:br-if)
  (:import-from #:cl-ppcre
                #:regex-replace-all))
(in-package :try-wasm-with-cl/wa/built-in-func)

(defvar *built-in-funcs* (make-hash-table))

(defun built-in-func-p (sym)
  (gethash sym *built-in-funcs*))

(defun convert-built-in-func (sym)
  (assert (built-in-func-p sym))
  (gethash sym *built-in-funcs*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sym-to-sym-for-print (sym)
    (intern (string-downcase
             (regex-replace-all "-" (symbol-name sym) "_")))))

(defmacro def-built-in-func (sym)
  `(progn (defvar ,sym nil)
          (setf (gethash ',sym *built-in-funcs*)
                ',(sym-to-sym-for-print sym))))

(def-built-in-func i32.add)
(def-built-in-func i32.const)
(def-built-in-func i32.eqz)
(def-built-in-func i32.ge-u)

(def-built-in-func get-local)
(def-built-in-func set-local)

(def-built-in-func br)
(def-built-in-func br-if)

(defpackage :try-wasm-with-cl/wa/environment
  (:use :cl)
  (:export :wsymbol-function
           :wsymbol-macro-function
           :wsymbol-import
           :intern.wat
           :wenv-function-symbols
           :wenv-macro-function-symbols
           :wenv-import-symbols
           :wenv-function-body-generators
           :wenv-import-body-generators)
  (:import-from :alexandria
                :hash-table-values))
(in-package :try-wasm-with-cl/wa/environment)

;; --- wat-symbol --- ;;

(defstruct wat-symbol
  symbol
  import
  function
  macro-function)

(defun set-function-empty (wsymbol)
  (when (wat-symbol-function wsymbol)
    (warn "~A has been defined as WAT function"
          (wat-symbol-symbol wsymbol)))
  (setf (wat-symbol-function wsymbol) nil))

(defun set-macro-function-empty (wsymbol)
  (when (wat-symbol-macro-function wsymbol)
    (warn "~A has been defined as WAT macro function"
          (wat-symbol-symbol wsymbol)))
  (setf (wat-symbol-macro-function wsymbol) nil))

(defun set-import-empty (wsymbol)
  (when (wat-symbol-import wsymbol)
    (warn "~A has been defined as WAT import"
          (wat-symbol-symbol wsymbol)))
  (setf (wat-symbol-import wsymbol) nil))

(defsetf wsymbol-function (wsymbol) (func)
  `(progn (setf (wat-symbol-function ,wsymbol) ,func)
          (set-macro-function-empty ,wsymbol)
          (set-import-empty ,wsymbol)
          ,wsymbol))

(defsetf wsymbol-macro-function (wsymbol) (macro-func)
  `(progn (set-function-empty ,wsymbol)
          (setf (wat-symbol-macro-function ,wsymbol) ,macro-func)
          (set-import-empty ,wsymbol)
          ,wsymbol))

(defsetf wsymbol-import (wsymbol) (import)
  `(progn (set-function-empty ,wsymbol)
          (set-macro-function-empty ,wsymbol)
          (setf (wat-symbol-import ,wsymbol) ,import)
          ,wsymbol))

;; --- wat-environment --- ;;

(defstruct wat-environment
  (symbol-to-wat-symbols (make-hash-table)))

(defvar *global-wat-env* (make-wat-environment))

(defun intern.wat (sym)
  (let ((table (wat-environment-symbol-to-wat-symbols
                *global-wat-env*)))
    (multiple-value-bind (wsym found) (gethash sym table)
      (when found
        (return-from intern.wat wsym))
      (setf (gethash sym table)
            (make-wat-symbol :symbol sym)))))

;; - symbols getter - ;;

(defun extract-wsymbols-by-accessor (wenv wsym-accessor)
  (remove-if (lambda (wsym)
               (not (funcall wsym-accessor wsym)))
             (hash-table-values
              (wat-environment-symbol-to-wat-symbols wenv))))

(defun wenv-specified-symbols (wenv wsym-accessor)
  (mapcar #'wat-symbol-symbol
          (extract-wsymbols-by-accessor wenv wsym-accessor)))

(defun wenv-function-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-function))

(defun wenv-macro-function-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-macro-function))

(defun wenv-import-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-import))

;; - body getter - ;;

(defun wenv-function-body-generators (&optional (wenv *global-wat-env*))
  (mapcar #'wat-symbol-function
          (extract-wsymbols-by-accessor wenv #'wat-symbol-function)))

(defun wenv-import-body-generators (&optional (wenv *global-wat-env*))
  (mapcar #'wat-symbol-import
          (extract-wsymbols-by-accessor wenv #'wat-symbol-import)))

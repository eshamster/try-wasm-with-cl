(defpackage :try-wasm-with-cl/wa/body-parser
  (:use :cl)
  (:export :parse-body)
  (:import-from :try-wasm-with-cl/wa/environment
                :intern.wat
                :wenv-function-symbols
                :wenv-import-symbols
                :wsymbol-macro-function)
  (:import-from :try-wasm-with-cl/wa/defmacro
                :macroexpand.wat)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :local)
  (:import-from :try-wasm-with-cl/wa/type
                :convert-type)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name))
(in-package :try-wasm-with-cl/wa/body-parser)

;; --- vars --- ;;

(defstruct vars
  (lst (list)))

(defun push-var (var vars)
  (push var (vars-lst vars)))

(defun find-var-in (var vars)
  (find var (vars-lst vars)))

;; --- parser --- ;;

(defun parse-body (body args)
  (let* ((vars (append args
                       (wenv-function-symbols)
                       (wenv-import-symbols))))
    (flatten-progn-all
     (parse-form body (make-vars :lst vars)))))

(defun flatten-progn-all (body)
  ;; Ex. ((progn 1 2) 3 (progn 4 (progn 5))) -> (1 2 3 4 5)
  (labels ((progn-p (target)
             (and (listp target)
                  (eq (car target) 'progn)))
           (rec (rest)
             (cond ((atom rest)
                    rest)
                   (t (mapcan (lambda (unit)
                                (if (progn-p unit)
                                    (rec (cdr unit))
                                    (list (rec unit))))
                              rest)))))
    (rec body)))

(defun parse-form (form vars)
  (cond ((atom form)
         (parse-atom form vars))
        ((special-form-p form)
         (parse-special-form form vars))
        ((macro-form-p form)
         (parse-macro-form form vars))
        (t (mapcar (lambda (unit)
                     (parse-form unit vars))
                   form))))

(defun parse-atom (atom vars)
  (if (find-var-in atom vars)
      (parse-arg-name atom)
      atom))

;; - special form - ;;

(defun parse-special-form (form vars)
  (ecase (car form)
    (progn `(progn ,@(mapcar (lambda (unit)
                               (parse-form unit vars))
                             (cdr form))))
    (local (destructuring-bind (var type) (cdr form)
             (push-var var vars)
             `(|local| ,(parse-atom var vars)
                       ,(convert-type type))))))

(defun special-form-p (form)
  (case (car form)
    ((progn local)
     t)
    (t nil)))

;; - macro - ;;

(defun parse-macro-form (form vars)
  ;; TODO: consider environment
  (parse-form (macroexpand.wat form) vars))

(defun macro-form-p (form)
  (wsymbol-macro-function (intern.wat (car form))))

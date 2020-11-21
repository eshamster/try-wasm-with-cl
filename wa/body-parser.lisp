(defpackage :try-wasm-with-cl/wa/body-parser
  (:use :cl)
  (:export :parse-body)
  (:import-from :try-wasm-with-cl/wa/environment
                :wsymbol-var
                :*global-wat-env*
                :intern.wat
                :clone-wenvironment
                :wenv-function-symbols
                :wenv-import-symbols
                :wenv-var-symbols
                :wsymbol-macro-function)
  (:import-from :try-wasm-with-cl/wa/defmacro
                :macroexpand.wat)
  (:import-from :try-wasm-with-cl/wa/reserved-word
                :|local|
                :local
                :|block|
                :block
                :|loop|
                :loop)
  (:import-from :try-wasm-with-cl/wa/type
                :convert-type)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name))
(in-package :try-wasm-with-cl/wa/body-parser)

;; --- local environment --- ;;

(defvar *org-global-wat-env* nil)

(defun var-p (sym)
  (some (lambda (syms)
          (find sym syms))
        (list (wenv-var-symbols)
              (wenv-function-symbols)
              (wenv-import-symbols))))

;; --- parser --- ;;

(defun parse-body (body args)
  (let ((*org-global-wat-env* *global-wat-env*)
        (*global-wat-env* (clone-wenvironment)))
    (dolist (arg args)
      (setf (wsymbol-var (intern.wat arg)) t))
    (flatten-progn-all
     (parse-form body))))

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

(defun parse-form (form)
  (cond ((atom form)
         (parse-atom form))
        ((special-form-p form)
         (parse-special-form form))
        ((macro-form-p form)
         (parse-macro-form form))
        (t (mapcar (lambda (unit)
                     (parse-form unit))
                   form))))

(defun parse-atom (atom)
  (if (var-p atom)
      (parse-arg-name atom)
      atom))

;; - special form - ;;

(defun parse-special-form (form)
  (ecase (car form)
    (progn `(progn ,@(mapcar (lambda (unit)
                               (parse-form unit))
                             (cdr form))))
    (local (destructuring-bind (var type) (cdr form)
             (setf (wsymbol-var (intern.wat var)) t)
             `(|local| ,(parse-atom var)
                       ,(convert-type type))))
    (block (destructuring-bind (var &rest rest-form) (cdr form)
             (setf (wsymbol-var (intern.wat var)) t)
             `(|block| ,(parse-atom var)
                       ,@(parse-form rest-form))))
    (loop (destructuring-bind (var &rest rest-form) (cdr form)
             (setf (wsymbol-var (intern.wat var)) t)
             `(|loop| ,(parse-atom var)
                      ,@(parse-form rest-form))))))

(defun special-form-p (form)
  (case (car form)
    ((progn local block loop)
     t)
    (t nil)))

;; - macro - ;;

(defun parse-macro-form (form)
  ;; TODO: consider environment
  (parse-form (macroexpand.wat form *org-global-wat-env*)))

(defun macro-form-p (form)
  (wsymbol-macro-function (intern.wat (car form))))

(defpackage :try-wasm-with-cl/wa/body-parser
  (:use :cl)
  (:export :parse-body)
  (:import-from :try-wasm-with-cl/wa/environment
                :wenv-function-symbols
                :wenv-import-symbols)
  (:import-from :try-wasm-with-cl/wa/utils
                :parse-arg-name))
(in-package :try-wasm-with-cl/wa/body-parser)

(defun parse-body (body args)
  (let* ((vars (append args
                       (wenv-function-symbols)
                       (wenv-import-symbols))))
    (flatten-progn-all
     (parse-form body vars))))

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
        (t (mapcar (lambda (unit)
                     (parse-form unit vars))
                   form))))

(defun parse-atom (atom vars)
  (if (find atom vars)
      (parse-arg-name atom)
      atom))

;; - special form - ;;

(defun parse-special-form (form vars)
  (ecase (car form)
    ('progn `(progn ,@(mapcar (lambda (unit)
                                (parse-body unit vars))
                              (cdr form))))))

(defun special-form-p (form)
  (case (car form)
    (('progn)
     t)
    (t nil)))

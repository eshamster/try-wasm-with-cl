(defpackage :try-wasm-with-cl/wa/default-macro
  (:use #:cl)
  (:export #:for
           #:i32+)
  (:import-from #:try-wasm-with-cl/wa/built-in-func
                #:set-local
                #:br
                #:br-if
                #:i32.const
                #:i32.add)
  (:import-from #:try-wasm-with-cl/wa/defmacro
                #:defmacro.wat)
  (:import-from #:try-wasm-with-cl/wa/reserved-word
                #:|if|
                #:|then|
                #:then
                #:|else|
                #:else
                #:local
                #:block
                #:loop)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :try-wasm-with-cl/wa/default-macro)

;; --- control macros --- ;;

(defmacro.wat if (test-form then-form &optional else-form)
  `(|if| ,test-form
         ,@(if else-form
               `((|then| ,then-form)
                 (|else| ,else-form))
               `((|then| ,then-form)))))

(defmacro.wat when (test-form &body form)
  `(if ,test-form
       (progn ,@form)))

(defmacro.wat cond (&rest clauses)
  (labels ((rec (rest-clauses)
             (unless rest-clauses
               (return-from rec))
             (let* ((clause (car rest-clauses))
                    (test-form (car clause))
                    (form (cdr clause)))
               (cond ((eq test-form 't)
                      `(progn ,@form))
                     ((cdr rest-clauses)
                      `(if ,test-form
                           (progn ,@form)
                           ,(rec (cdr rest-clauses))))
                     (t
                      `(if ,test-form
                           (progn ,@form)))))))
    (rec clauses)))

(defmacro.wat let (var-forms &body body)
  ;; Ex. (let (((i i32) (i32.const 0))
  ;;           (j i32))
  ;;       ...)
  ;; Limitations:
  ;; - Can use this only at head of function.
  ;; - A variable is not hidden to others.
  ;; - The scope is same to "local" operator.
  `(progn ,@(mapcar (lambda (var-form)
                      (let ((var-type (if (listp (car var-form))
                                          (car var-form)
                                          var-form)))
                        (destructuring-bind (var type) var-type
                          `(local ,var ,type))))
                    var-forms)
          ,(mapcan (lambda (var-form)
                     (when (listp (car var-form))
                       (let ((var-type (car var-form))
                             (init (cadr var-form)))
                         (destructuring-bind (var type) var-type
                           (declare (ignore type))
                           `(set-local ,var ,init)))))
                   var-forms)
          ,@body))

(defmacro.wat for (for-name (&key init break mod) &body body)
  (let ((block-name (symbolicate for-name "-BLOCK"))
        (loop-name  (symbolicate for-name "-LOOP")))
    `(progn ,init
            (block ,block-name
              (loop ,loop-name
                     (br-if ,block-name ,break)
                    ,@body
                    ,mod
                     (br ,loop-name))))))

;; --- calculation macros --- ;;

(defmacro def-calculation-macro (name const op)
  `(defmacro.wat ,name (&rest numbers)
     (flet ((parse-number (number)
              (if (numberp number)
                  `(,',const ,number)
                  number)))
       (case (length numbers)
         (0 `(,',const 0))
         (t (labels ((rec (rest-numbers)
                       (let ((head (car rest-numbers))
                             (rest (cdr rest-numbers)))
                         (if rest
                             `(,',op ,(parse-number head)
                                     ,(rec rest))
                             (parse-number head)))))
              (rec numbers)))))))

(def-calculation-macro i32+ i32.const i32.add)

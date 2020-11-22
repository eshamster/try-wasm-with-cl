(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use #:cl)
  (:import-from #:try-wasm-with-cl/wa/main
                #:defmacro.wat
                #:defun.wat
                #:defimport.wat
                #:defexport.wat

                #:local
                #:block
                #:loop

                #:br
                #:br-if

                #:i32.const
                #:i32.add
                #:i32.eqz
                #:i32.ge-u
                #:get-local
                #:set-local)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat log console.log (func ((i32))))

(defmacro.wat let (var-forms &body body)
  ;; Ex. (let (((i i32) (i32.const 0))
  ;;           (j i32))
  ;;       ...)
  ;; Can use this only at head of function
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

(defun.wat sample ((x i32)) (i32)
  (let (((tmp i32) (i32.const 100)))
    (i32.add (get-local x) (get-local tmp))))

(defun.wat test-if ((x i32)) ()
  (if (i32.eqz (get-local x))
      (log (i32.const 10))
      (log (i32.const 20))))

(defmacro.wat incf-i32 (place &optional (added '(i32.const 1)))
  `(progn (get-local ,place)
          ,added
          (i32.add)
          (set-local ,place)))

(defmacro.wat for (for-name params &body body)
  (let ((block-name (symbolicate for-name "-BLOCK"))
        (loop-name  (symbolicate for-name "-LOOP")))
    (destructuring-bind (&key init break mod) params
      `(progn ,init
              (block ,block-name
                (loop ,loop-name
                       (br-if ,block-name ,break)
                      ,@body
                      ,mod
                      (br ,loop-name)))))))

(defun.wat test-for () ()
  (let ((i i32)
        ((max i32) (i32.const 5)))
    (for f (:init (set-local i (i32.const 1))
            :break (i32.ge-u (get-local i) (get-local max))
            :mod (incf-i32 i))
         (log (get-local i)))))

(defun.wat test-print () ()
  (log (sample (i32.const 300)))
  ;; test-if
  (test-if (i32.const 0))
  (test-if (i32.const 1))
  ;; test-for
  (test-for))

(defexport.wat exported-func (func test-print))

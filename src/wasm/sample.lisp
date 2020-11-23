(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use #:cl)
  (:import-from #:try-wasm-with-cl/wa/main
                #:defmacro.wat
                #:defun.wat
                #:defimport.wat
                #:defexport.wat

                #:for

                #:i32.const
                #:i32.add
                #:i32.eq
                #:i32.eqz
                #:i32.ge-u
                #:get-local
                #:set-local)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat log console.log (func ((i32))))

(defun.wat sample ((x i32)) (i32)
  (let (((tmp i32) (i32.const 100)))
    (i32.add (get-local x) (get-local tmp))))

(defun.wat test-if ((x i32)) ()
  (if (i32.eqz (get-local x))
      (log (i32.const 10))
      (log (i32.const 20))))

(defun.wat test-cond ((x i32)) ()
  (cond ((i32.eq (get-local x)
                 (i32.const 1))
         (log (i32.const 111)))
        ((i32.eq (get-local x)
                 (i32.const 2))
         (log (i32.const 222)))
        (t (log (i32.const 999)))))

(defmacro.wat incf-i32 (place &optional (added '(i32.const 1)))
  `(progn (get-local ,place)
          ,added
          (i32.add)
          (set-local ,place)))

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
  ;; test-cond
  (test-cond (i32.const 1))
  (test-cond (i32.const 2))
  (test-cond (i32.const 3))
  ;; test-for
  (test-for))

(defexport.wat exported-func (func test-print))

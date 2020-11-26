(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use #:cl)
  (:import-from #:try-wasm-with-cl/wa/main
                #:defmacro.wat
                #:defun.wat
                #:defimport.wat
                #:defexport.wat
                #:defglobal.wat

                #:func
                #:memory
                #:mut

                #:for
                #:i32+

                #:i32.const
                #:i32.add
                #:i32.eq
                #:i32.eqz
                #:i32.ge-u
                #:i32.store
                #:i32.load
                #:get-local
                #:set-local
                #:get-global
                #:set-global)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat log console.log (func ((i32))))
(defimport.wat mem js.mem (memory 1))

(defglobal.wat g js.global (mut i32))

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

(defun.wat test-plus ((x i32)) ()
  (log (i32+))
  (log (i32+ (get-local x)))
  (log (i32+ (get-local x) 1))
  (log (i32+ (get-local x) 1 2)))

(defun.wat test-memory () ()
  (i32.store (i32.const 5)
             (i32.const 111))
  (log (i32.load (i32.const 5))))

(defun.wat test-global () ()
  (set-global g (i32.const 99))
  (log (get-global g)))

(defun.wat test-print () ()
  (log (sample (i32.const 300)))
  (test-if (i32.const 0))
  (test-if (i32.const 1))
  (test-cond (i32.const 1))
  (test-cond (i32.const 2))
  (test-cond (i32.const 3))
  (test-for)
  (test-plus (i32.const 100))
  (test-memory)
  (test-global))

(defexport.wat exported-func (func test-print))

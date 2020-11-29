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
                #:i32-

                #:i32.const
                #:i32.add
                #:i32.sub
                #:i32.mul
                #:i32.rem-u
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

(defun.wat get-global-memory-head () (i32)
  (i32.const 1))

(defun.wat get-header-size () (i32)
  (i32.const 1))

(defun.wat init-memory () ()
  (i32.store (get-global-memory-head)
             (i32.const 2))
  (i32.store (i32.const 2)
             (i32.const 0)))

(defun.wat get-empty-memory-size ((head i32)) (i32)
  (i32.load (i32+ (get-local head) 1)))

(defun.wat set-empty-memory-size ((head i32) (size i32)) ()
  (i32.store (i32+ (get-local head) 1)
             (get-local size)))

(defun.wat get-header-of-pointer ((ptr i32)) (i32)
  (i32.load (i32- (get-local ptr) 1)))

(defun.wat get-next-head ((head i32)) (i32)
  (i32.load (get-local head)))

(defun.wat malloc-rec ((size i32) (pre-head i32) (head i32)) (i32)
  (let (((next-head i32) (get-next-head (get-local head)))
        (new-head i32)
        (rest-size i32)
        (result i32))
    (cond
      ;; --- Case of tail of memory.
      ((i32.eq (get-local next-head) (i32.const 0))
       ;; TODO: Extend memory if shortage
       (set-local new-head
                  (i32+ (get-local head)
                        (get-local size)
                        (get-header-size)))
       (i32.store (get-local new-head)
                  (i32.const 0))
       (i32.store (get-local pre-head)
                  (get-local new-head))
       (i32.store (get-local head)
                  (get-local size))
       (set-local result (i32+ (get-local head)
                               (get-header-size))))
      ;; --- Case where empty size is enough.
      ((i32.ge-u (get-empty-memory-size (get-local head))
                 (get-local size))
       (set-local rest-size (i32- (get-empty-memory-size (get-local head))
                                  (get-local size)))
       (if (i32.eqz (get-local rest-size))
           (set-local new-head (get-local next-head))
           (progn (set-local new-head (i32+ (get-local head)
                                            (get-local size)
                                            (get-header-size)))
                  (i32.store (get-local new-head)
                             (get-local next-head))
                  ;; Assume that area to store size is remained.
                  ;; (It should be ensured by alignement)
                  (set-empty-memory-size (get-local new-head)
                                         (i32- (get-local rest-size)
                                               (get-header-size)))))
       (i32.store (get-local pre-head)
                  (get-local new-head))
       (i32.store (get-local head)
                  (get-local size))
       (set-local result (i32+ (get-local head)
                               (get-header-size))))
      ;; --- Case where empty size is not enough.
      (t (malloc-rec (get-local size)
                     (get-local head)
                     (get-local next-head))
         (set-local result))))
  (get-local result))

(defun.wat adjust-malloc-size ((size i32) (header-size i32) (align-size i32)) (i32)
  (let* (((required i32) (i32+ (get-local size)
                               (get-local header-size)))
         ((rem i32) (i32.rem-u (get-local required)
                               (get-local align-size)))
         (aligned i32))
    (if (i32.eqz (get-local rem))
        (set-local aligned (get-local required))
        (set-local aligned (i32+ (get-local required)
                                 (i32- (get-local align-size)
                                       (get-local rem)))))
    (i32- (get-local aligned) (get-local header-size))))

;; Allocate memory and return offset of its head
(defun.wat malloc ((size i32)) (i32)
  (let (((actual-size i32) (adjust-malloc-size (get-local size)
                                               (get-header-size)
                                               (i32.const 2)))
        ((global-head i32) (get-global-memory-head)))
    (malloc-rec (get-local actual-size)
                (get-local global-head)
                (get-next-head (get-local global-head)))))

(defun.wat free ((ptr i32)) ()
  )

(defun.wat test-mem-process () ()
  ;; expect 16 - 2
  (log (adjust-malloc-size (i32.const 12) (i32.const 2) (i32.const 8)))
  ;; expect 16 - 2
  (log (adjust-malloc-size (i32.const 14) (i32.const 2) (i32.const 8)))
  (init-memory)
  ;; expect 3
  (log (malloc (i32.const 14))))

(defexport.wat test-memory (func test-mem-process))

;; --- --- ;;

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

;; factorial
(defun.wat test-rec ((x i32)) (i32)
  (let ((result i32))
    (if (i32.ge-u (i32.const 1) (get-local x))
        (set-local result (i32.const 1))
        (progn (i32.mul (get-local x)
                        (test-rec (i32.sub (get-local x) (i32.const 1))))
               (set-local result)))
    (get-local result)))

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
  (test-global)
  (log (test-rec (i32.const 5))))

(defexport.wat exported-func (func test-print))

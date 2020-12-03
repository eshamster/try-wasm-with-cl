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
                #:i32.gt-u
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

;; Note: The offset of i32.store and i32.load is by 8 bits.
;; Instead, use store-i32 and load-i32 to process offset by 32 bits.

(defun.wat store-i32 ((offset i32) (value i32)) ()
  (i32.store (i32.mul (get-local offset)
                      (i32.const 4))
             (get-local value)))

(defun.wat load-i32 ((offset i32)) (i32)
  (i32.load (i32.mul (get-local offset)
                     (i32.const 4))))

(defun.wat get-global-memory-head () (i32)
  (i32.const 1))

(defun.wat global-memory-head-p ((head i32)) (i32)
  (i32.eq (get-local head) (i32.const 1)))

(defun.wat last-empty-head-p ((head i32)) (i32)
  (i32.eqz (load-i32 (get-local head))))

(defun.wat get-header-size () (i32)
  (i32.const 1))

(defun.wat init-memory () ()
  (store-i32 (get-global-memory-head)
             (i32.const 2))
  (store-i32 (i32.const 2)
             (i32.const 0)))

(defun.wat get-empty-memory-size ((head i32)) (i32)
  (load-i32 (i32+ (get-local head) 1)))

(defun.wat set-empty-memory-size ((head i32) (size i32)) ()
  (store-i32 (i32+ (get-local head) 1)
             size))

(defun.wat get-header-of-pointer ((ptr i32)) (i32)
  (load-i32 (i32- (get-local ptr) 1)))

(defun.wat get-next-head ((head i32)) (i32)
  (load-i32 head))

(defun.wat get-pointer-size ((ptr i32)) (i32)
  (load-i32 (i32- (get-local ptr) 1)))

;; - malloc - ;;

(defun.wat malloc-rec ((size i32) (prev-head i32) (head i32)) (i32)
  (let (((next-head i32) (get-next-head head))
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
       (store-i32 new-head (i32.const 0))
       (store-i32 prev-head new-head)
       (store-i32 head size)
       (set-local result (i32+ (get-local head)
                               (get-header-size))))
      ;; --- Case where empty size is enough.
      ((i32.ge-u (get-empty-memory-size head)
                 (get-local size))
       (set-local rest-size (i32- (get-empty-memory-size head)
                                  (get-local size)))
       (if (i32.eqz (get-local rest-size))
           (set-local new-head (get-local next-head))
           (progn (set-local new-head (i32+ (get-local head)
                                            (get-local size)
                                            (get-header-size)))
                  (store-i32 new-head next-head)
                  ;; Assume that area to store size is remained.
                  ;; (It should be ensured by alignement)
                  (set-empty-memory-size new-head
                                         (i32- (get-local rest-size)
                                               (get-header-size)))))
       (store-i32 prev-head new-head)
       (store-i32 head size)
       (set-local result (i32+ (get-local head)
                               (get-header-size))))
      ;; --- Case where empty size is not enough.
      (t (malloc-rec size head next-head)
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
  (let (((actual-size i32) (adjust-malloc-size size
                                               (get-header-size)
                                               (i32.const 2)))
        ((global-head i32) (get-global-memory-head)))
    (malloc-rec actual-size
                global-head
                (get-next-head global-head))))

;; - free - ;;

(defun.wat find-prev-empty-head-rec ((ptr i32) (head i32)) (i32)
  (let (((next-head i32) (get-next-head head))
        (result i32))
    (cond ((i32.eqz (get-local next-head))
           ;; This case should not happen
           (set-local result (i32.const 0)))
          ((i32.gt-u (get-local next-head) (get-local ptr))
           (set-local result (get-local head)))
          (t (find-prev-empty-head-rec ptr next-head)
             (set-local result)))
    (get-local result)))

(defun.wat find-prev-empty-head ((ptr i32)) (i32)
  (find-prev-empty-head-rec ptr (get-global-memory-head)))

;; Return 1 if merge is enable, otherwize return 0.
(defun.wat merge-empty-memory-if-enable ((prev-head i32) (head i32)) (i32)
  (let (((result i32) (i32.const 0)))
    (unless (global-memory-head-p prev-head)
      (when (i32.eq (i32+ (get-local prev-head)
                          (get-header-size)
                          (get-empty-memory-size prev-head))
                    (get-local head))
        (store-i32 prev-head
                   (get-next-head head))
        (unless (last-empty-head-p head)
          (set-empty-memory-size prev-head
                                 (i32+ (get-empty-memory-size prev-head)
                                       (get-header-size)
                                       (get-empty-memory-size head))))
        (set-local result (i32.const 1))))
    (get-local result)))

(defun.wat free ((ptr i32)) ()
  (let* (((prev-head i32) (find-prev-empty-head ptr))
         ((next-head i32) (get-next-head prev-head))
         ((new-head i32) (i32- (get-local ptr)
                               (get-header-size)))
         ((size i32) (get-pointer-size (get-local ptr))))
    ;; register ptr as empty
    (store-i32 prev-head new-head)
    (store-i32 new-head next-head)
    (set-empty-memory-size new-head size)
    ;; merge into prev empty if enable
    (when (merge-empty-memory-if-enable prev-head new-head)
      (set-local new-head (get-local prev-head)))
    ;; merge into next empty if enable
    ;; (Use "when" to return no value)
    (when (merge-empty-memory-if-enable new-head next-head))))

;; - test - ;;

(defun.wat test-mem-process () ()
  (let ((mem1 i32)
        (mem2 i32)
        (mem3 i32))
    ;; expect 16 - 2
    (log (adjust-malloc-size (i32.const 12) (i32.const 2) (i32.const 8)))
    ;; expect 16 - 2
    (log (adjust-malloc-size (i32.const 14) (i32.const 2) (i32.const 8)))

    (init-memory)
    ;; - malloc - ;;
    ;; expect 3 - free space: 8~
    (set-local mem1 (malloc (i32.const 5)))
    (log mem1)
    ;; expect 9 - free space: 12~
    (set-local mem2 (malloc (i32.const 3)))
    (log mem2)
    ;; expect 13 - free space: 18~
    (set-local mem3 (malloc (i32.const 4)))
    (log mem3)
    (log (load-i32 (get-global-memory-head))) ; expect 18

    ;; - free - ;;
    ;; free space: 8~11, 18~
    (free mem2)
    (log (load-i32 (get-global-memory-head))) ; expect 8
    (log (load-i32 (i32.const 8))) ; expect 18
    (log (load-i32 (get-empty-memory-size (i32.const 8)))) ; expect 3
    ;; free space: 2~11, 18~
    (free mem1)
    (log (load-i32 (get-global-memory-head))) ; expect 2
    (log (load-i32 (i32.const 2))) ; expect 18
    (log (load-i32 (i32.const 3))) ; expect 5 + 1 + 3 = 9 (empty size)
    ;; free space: 2~
    (free mem3)
    (log (load-i32 (get-global-memory-head))) ; expect 2
    (log (last-empty-head-p (i32.const 2))) ; expect 1
    ))

(defexport.wat test-memory (func test-mem-process))

;; --- --- ;;

(defun.wat sample ((x i32)) (i32)
  (let (((tmp i32) (i32.const 100)))
    (i32.add (get-local x) (get-local tmp))))

(defun.wat test-if ((x i32)) ()
  (if (i32.eqz (get-local x))
      (log (i32.const 10))
      (log (i32.const 20))))

(defun.wat test-unless ((x i32)) ()
  (unless (i32.eqz (get-local x))
    (log (i32.const 30))))

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
         (log i))))

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
  (test-unless (i32.const 0))
  (test-unless (i32.const 1))
  (test-cond (i32.const 1))
  (test-cond (i32.const 2))
  (test-cond (i32.const 3))
  (test-for)
  (test-plus (i32.const 100))
  (test-memory)
  (test-global)
  (log (test-rec (i32.const 5))))

(defexport.wat exported-func (func test-print))

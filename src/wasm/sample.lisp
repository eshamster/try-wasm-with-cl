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

;; --- debug --- ;;

(defglobal.wat debug js.debug (mut i32))

(defmacro.wat with-debug (&body body)
  `(progn (set-global debug (i32.const 1))
          ,@body
          (set-global debug (i32.const 0))))

(defun.wat debug-p () (i32)
  (get-global debug))

;; --- memory allocation --- ;;

;; Note: The offset of i32.store and i32.load is by 8 bits.
;; Instead, use store-i32 and load-i32 to process offset by 32 bits.

(defun.wat store-i32 ((offset i32) (value i32)) ()
  (i32.store (i32.mul offset (i32.const 4))
             value))

(defun.wat load-i32 ((offset i32)) (i32)
  (i32.load (i32.mul offset (i32.const 4))))

(defun.wat get-null-ptr () (i32)
  (i32.const 0))

(defun.wat get-global-memory-head () (i32)
  (i32.const 1))

(defun.wat global-memory-head-p ((head i32)) (i32)
  (i32.eq head (i32.const 1)))

(defun.wat last-empty-head-p ((head i32)) (i32)
  (i32.eqz (load-i32 head)))

(defun.wat get-header-size () (i32)
  (i32.const 1))

(defun.wat init-memory () ()
  (store-i32 (get-global-memory-head)
             (i32.const 2))
  (store-i32 (i32.const 2)
             (i32.const 0)))

(defun.wat get-empty-memory-size ((head i32)) (i32)
  (load-i32 (i32+ head 1)))

(defun.wat set-empty-memory-size ((head i32) (size i32)) ()
  (store-i32 (i32+ head 1)
             size))

(defun.wat get-next-head ((head i32)) (i32)
  (load-i32 head))

(defun.wat get-pointer-size ((ptr i32)) (i32)
  (load-i32 (i32- ptr 1)))

;; - malloc - ;;

(defun.wat malloc-rec ((size i32) (prev-head i32) (head i32)) (i32)
  (let (((next-head i32) (get-next-head head))
        (new-head i32)
        (rest-size i32)
        (result i32))
    (cond
      ;; --- Case of tail of memory.
      ((i32.eq next-head (i32.const 0))
       ;; TODO: Extend memory if shortage
       (set-local new-head
                  (i32+ head size (get-header-size)))
       (store-i32 new-head (i32.const 0))
       (store-i32 prev-head new-head)
       (store-i32 head size)
       (set-local result (i32+ head (get-header-size))))
      ;; --- Case where empty size is enough.
      ((i32.ge-u (get-empty-memory-size head)
                 size)
       (set-local rest-size (i32- (get-empty-memory-size head)
                                  size))
       (if (i32.eqz rest-size)
           (set-local new-head next-head)
           (progn (set-local new-head (i32+ head size (get-header-size)))
                  (store-i32 new-head next-head)
                  ;; Assume that area to store size is remained.
                  ;; (It should be ensured by alignement)
                  (set-empty-memory-size new-head
                                         (i32- rest-size (get-header-size)))))
       (store-i32 prev-head new-head)
       (store-i32 head size)
       (set-local result (i32+ head (get-header-size))))
      ;; --- Case where empty size is not enough.
      (t (malloc-rec size head next-head)
         (set-local result))))
  (get-local result))

(defun.wat adjust-malloc-size ((size i32) (header-size i32) (align-size i32)) (i32)
  (let* (((required i32) (i32+ size header-size))
         ((rem i32) (i32.rem-u required align-size))
         (aligned i32))
    (if (i32.eqz rem)
        (set-local aligned required)
        (set-local aligned (i32+ required
                                 (i32- align-size rem))))
    (i32- aligned header-size)))

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
    (cond ((i32.eqz next-head)
           ;; This case should not happen
           (set-local result (i32.const 0)))
          ((i32.gt-u next-head ptr)
           (set-local result head))
          (t (find-prev-empty-head-rec ptr next-head)
             (set-local result)))
    (get-local result)))

(defun.wat find-prev-empty-head ((ptr i32)) (i32)
  (find-prev-empty-head-rec ptr (get-global-memory-head)))

;; Return 1 if merge is enable, otherwize return 0.
(defun.wat merge-empty-memory-if-enable ((prev-head i32) (head i32)) (i32)
  (let (((result i32) (i32.const 0)))
    (unless (global-memory-head-p prev-head)
      (when (i32.eq (i32+ prev-head
                          (get-header-size)
                          (get-empty-memory-size prev-head))
                    head)
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
         ((new-head i32) (i32- ptr (get-header-size)))
         ((size i32) (get-pointer-size ptr)))
    ;; register ptr as empty
    (store-i32 prev-head new-head)
    (store-i32 new-head next-head)
    (set-empty-memory-size new-head size)
    ;; merge into prev empty if enable
    (when (merge-empty-memory-if-enable prev-head new-head)
      (set-local new-head prev-head))
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

;; --- deftype.wat --- ;;

(defun.wat get-type-header-size () (i32)
  (i32.const 1))

(defun.wat get-type ((ptr i32)) (i32)
  (load-i32 ptr))

(defun.wat get-type-data-offset ((ptr i32)) (i32)
  (i32+ ptr (i32.const 1)))

(defmacro deftype.wat (name size id)
  ;; TODO: Automatically asign id
  `(progn (defun.wat ,(symbolicate "MAKE-" name) () (i32)
            (let (((ptr i32) (malloc (i32+ (get-type-header-size)
                                           (i32.const ,size)))))
              (store-i32 ptr (i32.const ,id))
              (get-local ptr)))
          (defun.wat ,(symbolicate name "-ID-P") ((typ i32)) (i32)
            (i32.eq typ (i32.const ,id)))
          (defun.wat ,(symbolicate name "-P") ((type-ptr i32)) (i32)
            (,(symbolicate name "-ID-P") (get-type type-ptr)))))

;; - i32 - ;;

(deftype.wat i32 1 1)

(defun.wat new-i32 ((value i32)) (i32)
  (let (((ptr i32) (make-i32)))
    (set-i32 ptr value)
    (get-local ptr)))

(defun.wat get-i32 ((i32-ptr i32)) (i32)
  (load-i32 (get-type-data-offset i32-ptr)))

(defun.wat set-i32 ((i32-ptr i32) (value i32)) ()
  (store-i32 (get-type-data-offset i32-ptr)
             value))

(defun.wat free-i32 ((i32-ptr i32)) ()
  (free i32-ptr))

;; - cons cell - ;;

;; storage 2 pointers
(deftype.wat cons-cell 2 101)

(defun.wat cons ((ptr-car i32) (ptr-cdr i32)) (i32)
  (let (((ptr i32) (make-cons-cell)))
    (set-car ptr ptr-car)
    (set-cdr ptr ptr-cdr)
    (get-local ptr)))

(defun.wat car ((cons-cell-ptr i32)) (i32)
  (load-i32 (get-type-data-offset cons-cell-ptr)))

(defun.wat set-car ((cons-cell-ptr i32) (value i32)) ()
  (store-i32 (get-type-data-offset cons-cell-ptr)
             value))

(defun.wat cdr ((cons-cell-ptr i32)) (i32)
  (load-i32 (i32+ (get-type-data-offset cons-cell-ptr)
                  1)))

(defun.wat set-cdr ((cons-cell-ptr i32) (value i32)) ()
  (store-i32 (i32+ (get-type-data-offset cons-cell-ptr)
                   (i32.const 1))
             value))

(defun.wat free-cons-cell ((cons-cell-ptr i32)) ()
  (free-typed (car cons-cell-ptr))
  (free-typed (cdr cons-cell-ptr))
  (free cons-cell-ptr))

;; - utils - ;;

(defun.wat atom ((type-ptr i32)) (i32)
  (let ((result i32))
    (cond ((i32-p type-ptr)
           (set-local result (i32.const 1)))
          (t
           (set-local result (i32.const 0))))
    (get-local result)))

(defun.wat free-typed ((type-ptr i32)) ()
  (cond ((i32-p type-ptr) (free-i32 type-ptr))
        ((cons-cell-p type-ptr) (free-cons-cell type-ptr))
        ((shared-ptr-p type-ptr) (deref-shared-ptr type-ptr))))

(defun.wat print-typed ((type-ptr i32)) ()
  (cond ((i32.eqz type-ptr))
        ((i32-p type-ptr)
         (log (get-i32 type-ptr)))
        ((cons-cell-p type-ptr)
         (print-typed (car type-ptr))
         (print-typed (cdr type-ptr)))
        ((shared-ptr-p type-ptr)
         (with-destruct (type-ptr)
           (print-typed (shared-ptr-ptr type-ptr))))))

;; - test - ;;

(defun.wat test-list () ()
  (let ((simple-cons i32)
        (lst i32)
        (test-ptr i32))
    ;; simple cons cell
    (set-local simple-cons
               (cons (new-i32 (i32.const 1))
                     (new-i32 (i32.const 2))))
    (print-typed simple-cons)
    ;; list
    (set-local lst (cons (new-i32 (i32.const 10))
                         (cons (new-i32 (i32.const 20))
                               (new-i32 (i32.const 30)))))
    (print-typed lst)
    ;; free
    (log (load-i32 (get-global-memory-head))) ; expect not 2
    (free-typed simple-cons)
    (free-typed lst)
    (log (load-i32 (get-global-memory-head))) ; expect 2
    ))

(defexport.wat test-list (func test-list))

;; --- shared-ptr --- ;;

(deftype.wat shared-ptr 2 999)

(defun.wat new-shared-ptr ((ptr i32)) (i32)
  (let (((result i32) (make-shared-ptr)))
    (set-shared-ptr-ref-count result (i32.const 1))
    (store-i32 (i32+ (get-type-data-offset result)
                     1)
               ptr)
    (get-local result)))

(defun.wat ref-shared-ptr ((shared-ptr i32)) (i32)
  (let (((ref-count i32) (shared-ptr-ref-count shared-ptr)))
    (set-shared-ptr-ref-count shared-ptr
                              (i32+ ref-count 1))
    (get-local shared-ptr)))

(defun.wat deref-shared-ptr ((shared-ptr i32)) ()
  (let (((ref-count i32) (shared-ptr-ref-count shared-ptr)))
    (if (i32.eq ref-count (i32.const 1))
        (progn (when (debug-p)
                 (log (i32.const 9999999)))
               (free-typed (shared-ptr-ptr shared-ptr))
               (free shared-ptr))
        (set-shared-ptr-ref-count shared-ptr
                                  (i32- ref-count 1)))))

(defun.wat shared-ptr-ref-count ((shared-ptr i32)) (i32)
  (load-i32 (get-type-data-offset shared-ptr)))

(defun.wat set-shared-ptr-ref-count ((shared-ptr i32) (ref-count i32)) ()
  (store-i32 (get-type-data-offset shared-ptr)
             ref-count))

(defun.wat shared-ptr-ptr ((shared-ptr i32)) (i32)
  (load-i32 (i32+ (get-type-data-offset shared-ptr)
                  1)))

(defmacro.wat set-local-shared-ptr (place shared-ptr)
  `(progn (when (shared-ptr-p ,place)
            (deref-shared-ptr ,place))
          ;; Note: Use "when" to ignore return value
          (when (ref-shared-ptr ,shared-ptr))
          (set-local ,place ,shared-ptr)))

(defun.wat destruct ((type-ptr i32)) ()
  (cond ((shared-ptr-p type-ptr)
         (deref-shared-ptr type-ptr))))

(defmacro.wat with-destruct (type-ptr-lst &body body)
  ;; Note: with-destruct doesn't return nothing.
  `(progn ,@body
          ,@(mapcar (lambda (type-ptr)
                      `(destruct ,type-ptr))
                    type-ptr-lst)))

(defun.wat test-shared-ptr1 () ()
  (let (((temp1 i32) (new-shared-ptr
                      (new-i32 (i32.const 100))))
        ((temp2 i32) (new-shared-ptr
                       (new-i32 (i32.const 200)))))
    (with-destruct (temp1 temp2)
      (print-typed (ref-shared-ptr temp2))
      (set-local-shared-ptr temp2 temp1)
      (log (i32.const 111111))
      (print-typed (ref-shared-ptr temp1))
      (print-typed (ref-shared-ptr temp2))
      (log (i32.const 111222)))
    (log (i32.const 111333))))

(defun.wat cons.sp ((ptr1 i32) (ptr2 i32)) (i32)
  (new-shared-ptr
   (cons (new-shared-ptr ptr1)
         (new-shared-ptr ptr2))))

(defun.wat test-shared-ptr2 () ()
  (let (((lst i32) (cons.sp (new-i32 (i32.const 1))
                            (new-i32 (i32.const 2)))))
    (with-destruct (lst)
      (log (i32.const 222111))
      (print-typed (ref-shared-ptr lst)))
    (log (i32.const 222222))))

(defun.wat test-shared-ptr3-called ((sp i32)) ()
  (with-destruct (sp)
    (log (i32.const 333999))
    (print-typed (ref-shared-ptr sp)))
  (log (i32.const 333888)))

(defun.wat test-shared-ptr3 () ()
  (let (((sp i32) (new-shared-ptr (new-i32 (i32.const 1)))))
    (with-destruct (sp)
      (log (i32.const 333111))
      (test-shared-ptr3-called (ref-shared-ptr sp))
      (log (i32.const 333222)))
    (log (i32.const 333333))))

(defun.wat test-shared-ptr () ()
  (with-debug
    (test-shared-ptr1)
    (test-shared-ptr2)
    (test-shared-ptr3))
  (log (load-i32 (get-global-memory-head))) ; expect 2
  )

(defexport.wat test-shared-ptr (func test-shared-ptr))

;; --- --- ;;

(defun.wat sample ((x i32)) (i32)
  (let (((tmp i32) (i32.const 100)))
    (i32.add x tmp)))

(defun.wat test-if ((x i32)) ()
  (if (i32.eqz x)
      (log (i32.const 10))
      (log (i32.const 20))))

(defun.wat test-unless ((x i32)) ()
  (unless (i32.eqz x)
    (log (i32.const 30))))

(defun.wat test-cond ((x i32)) ()
  (cond ((i32.eq x (i32.const 1))
         (log (i32.const 111)))
        ((i32.eq x (i32.const 2))
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
            :break (i32.ge-u i max)
            :mod (incf-i32 i))
         (log i))))

(defun.wat test-plus ((x i32)) ()
  (log (i32+))
  (log (i32+ x))
  (log (i32+ x 1))
  (log (i32+ x 1 2)))

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
    (if (i32.ge-u (i32.const 1) x)
        (set-local result (i32.const 1))
        (progn (i32.mul x
                        (test-rec (i32.sub x (i32.const 1))))
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

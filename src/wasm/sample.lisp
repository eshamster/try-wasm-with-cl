(defpackage :try-wasm-with-cl/src/wasm/sample
  (:use #:cl)
  (:import-from #:watson
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

                #:i32

                #:i32.const
                #:i32.add
                #:i32.sub
                #:i32.mul
                #:i32.rem-u
                #:i32.eq
                #:i32.eqz
                #:i32.ge-u
                #:i32.gt-u
                #:i32.lt-u
                #:i32.store
                #:i32.store8
                #:i32.load
                #:get-local
                #:set-local
                #:get-global
                #:set-global)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:flexi-streams
                #:string-to-octets))
(in-package :try-wasm-with-cl/src/wasm/sample)

(defimport.wat log console.log (func ((i32))))
(defimport.wat logs console.logs (func ((i32) (i32))))
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

(defun.wat null-ptr-p ((ptr i32)) (i32)
  (i32.eq ptr (i32.const 0)))

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

;; mainly for test
(defun.wat no-memory-allocated-p () (i32)
  (let (((expected-head i32) (i32.const 2))
        ((result i32) (i32.const 0)))
    (when (i32.eq (load-i32 (get-global-memory-head))
                  expected-head)
      (when (last-empty-head-p expected-head)
        (set-local result (i32.const 1))))
    (get-local result)))

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

;; --- log string --- ;;

(defmacro.wat log-string (text var-ptr)
  (unless (stringp text)
    (error "input should be string. got: ~A" text))
  ;; Note:
  ;; - logs/i32.store8 use memory by 8bit
  ;; - malloc use memory by 32bit
  (let* ((octets (string-to-octets text :external-format :utf-8))
         (alloc-size (ceiling (/ (length octets) 4))))
    `(progn (set-local ,var-ptr (malloc (i32.const ,alloc-size)))
            ,@(loop :for i :from 0 :below (length octets)
                    :collect `(i32.store8 (i32.add (i32.mul ,var-ptr
                                                           (i32.const 4))
                                                  (i32.const ,i))
                                         (i32.const ,(aref octets i))))
            (logs (i32.mul ,var-ptr (i32.const 4))
                  (i32.const ,(length octets)))
            (free ,var-ptr))))

(defun.wat test-log-string () ()
  (let ((ptr i32))
    (init-memory)
    (log-string "testaaaaa" ptr)
    (log-string "|斑|鳩|" ptr))
  (log (last-empty-head-p (i32.const 2))) ; expect 1
  )

(defexport.wat test-log-string (func test-log-string))

;; --- dump memory --- ;;

(defun.wat dump-empty-memory ((head i32)) ()
  (let ((tmp-for-log i32))
    (log-string "Empty(head,size):" tmp-for-log)
    (log head)
    (unless (last-empty-head-p head)
      (log (get-empty-memory-size head)))))

(defun.wat dump-allocated-memory ((ptr i32)) ()
  (let (((size i32) (get-pointer-size ptr))
        (i i32)
        (tmp-for-log i32))
    (log-string "Alloc(ptr,size,data...)" tmp-for-log)
    (log ptr)
    (log size)
    (for f (:init (set-local i (i32.const 0))
            :break (i32.ge-u i size)
            :mod (set-local i (i32+ i (i32.const 1))))
         (log (load-i32 (i32+ ptr i))))))

(defun.wat dump-allocated-memory-rec ((ptr i32) (next-head i32)) ()
  (let (((next-offset i32) (i32+ ptr (get-pointer-size ptr))))
    (dump-allocated-memory ptr)
    (when (i32.lt-u next-offset next-head)
      (dump-allocated-memory-rec (i32+ next-offset (get-header-size))
                                 next-head))))

(defun.wat dump-memory-rec ((head i32)) ()
  (let ((ptr i32)
        (next-head i32))
    (dump-empty-memory head)
    (unless (last-empty-head-p head)
      (set-local ptr (i32+ head
                           (get-empty-memory-size head)
                           (i32.const 1)
                           (get-header-size)))
      (set-local next-head (get-next-head head))
      (dump-allocated-memory-rec ptr next-head)
      (dump-memory-rec next-head))))

(defun.wat dump-memory () ()
  (let (((next-offset i32) (i32+ (get-global-memory-head) 1))
        ((next-head i32) (get-next-head (get-global-memory-head)))
        (tmp-for-log i32))
    (unless (i32.eq next-head next-offset)
      (dump-allocated-memory-rec (i32+ next-offset (get-header-size))
                                 next-head))
    (dump-memory-rec next-head)
    (log-string "--- End ---" tmp-for-log)))

(defun.wat test-dump-memory () ()
  (let ((ptr1 i32)
        (ptr2 i32))
    (init-memory)
    (set-local ptr1 (malloc (i32.const 4)))
    (set-local ptr2 (malloc (i32.const 5)))
    (free ptr1)
    (dump-memory)
    (init-memory)))

(defexport.wat test-dump-memory (func test-dump-memory))

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
  (let ((tmp-for-log i32))
    (cond ((null-ptr-p type-ptr) ; do nothing
           )
          ((i32-p type-ptr) (free-i32 type-ptr))
          ((cons-cell-p type-ptr) (free-cons-cell type-ptr))
          ((shared-ptr-p type-ptr) (deref-shared-ptr type-ptr))
          ((symbol-p type-ptr) (free-symbol type-ptr))
          ((env-p type-ptr) (free-env type-ptr))
          ((var-cell-p type-ptr) (free-var-cell type-ptr))
          (t (log-string "Can't free type: " tmp-for-log)
             (log (get-type type-ptr))))))

(defun.wat print-typed-rec ((type-ptr i32)) ()
  (let ((temp i32)
        (tmp-for-log i32))
    (cond ((i32.eqz type-ptr))
          ((i32-p type-ptr)
           (log (get-i32 type-ptr)))
          ((cons-cell-p type-ptr)
           (print-typed-rec (car type-ptr))
           (print-typed-rec (cdr type-ptr)))
          ((shared-ptr-p type-ptr)
           ;; Without destruct
           (print-typed-rec (shared-ptr-ptr type-ptr)))
          (t (log-string "Can't print type: " tmp-for-log)
             (log (get-type type-ptr))))))

(defun.wat print-typed ((type-ptr i32)) ()
  (cond ((shared-ptr-p type-ptr)
         (with-destruct (type-ptr)
           (print-typed-rec type-ptr)))
        (t (print-typed-rec type-ptr))))

(defun.wat eq-typed ((type-ptr1 i32) (type-ptr2 i32)) (i32)
  (let ((result i32))
    (if (i32.eq (get-type type-ptr1)
                (get-type type-ptr2))
        (cond ((i32-p type-ptr1)
               (set-local result
                          (i32.eq (get-i32 type-ptr1)
                                  (get-i32 type-ptr2))))
              ((symbol-p type-ptr1)
               (set-local result (eq-symbol type-ptr1 type-ptr2)))
              ((shared-ptr-p type-ptr1)
               (with-destruct (type-ptr1 type-ptr2)
                 (set-local result
                            (eq-typed (shared-ptr-ptr type-ptr1)
                                      (shared-ptr-ptr type-ptr2)))))
              (t (set-local result
                            (i32.eq type-ptr1 type-ptr2))))
        (set-local result (i32.const 0)))
    (get-local result)))

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
    (log (no-memory-allocated-p)) ; expect 0
    (free-typed simple-cons)
    (free-typed lst)
    (log (no-memory-allocated-p)) ; expect 1
    ))

(defexport.wat test-list (func test-list))

;; --- shared-ptr --- ;;

(deftype.wat shared-ptr 2 999)

(defun.wat new-shared-ptr ((ptr i32)) (i32)
  (let ((result i32))
    (if (shared-ptr-p ptr)
        (set-local result ptr)
        (progn (set-local result (make-shared-ptr))
               (set-shared-ptr-ref-count result (i32.const 1))
               (store-i32 (i32+ (get-type-data-offset result)
                                1)
                          ptr)))
    (get-local result)))

(defmacro.wat sp (ptr)
  `(new-shared-ptr ,ptr))

(defun.wat ref-shared-ptr ((shared-ptr i32)) (i32)
  (let (((ref-count i32) (shared-ptr-ref-count shared-ptr)))
    (set-shared-ptr-ref-count shared-ptr
                              (i32+ ref-count 1))
    (get-local shared-ptr)))

(defun.wat deref-shared-ptr ((shared-ptr i32)) ()
  (let (((ref-count i32) (shared-ptr-ref-count shared-ptr))
        (tmp-for-log i32))
    (if (i32.eq ref-count (i32.const 1))
        (progn (when (debug-p)
                 (log-string "free: " tmp-for-log)
                 (log shared-ptr))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ref-shared-ptr-reader (stream &rest rest)
    (declare (ignore rest))
    (let ((sym (read stream)))
      `(ref-shared-ptr ,sym)))
  (defun shared-ptr-ptr-reader (stream &rest rest)
    (declare (ignore rest))
    (let ((sym (read stream)))
      `(shared-ptr-ptr ,sym))))

(defmacro enable-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (make-dispatch-macro-character #\$)
    (set-dispatch-macro-character #\$ #\& #'ref-shared-ptr-reader)
    (set-dispatch-macro-character #\$ #\* #'shared-ptr-ptr-reader)))

(enable-syntax)

(defun.wat test-shared-ptr1 () ()
  (let (((temp1 i32) (new-shared-ptr
                      (new-i32 (i32.const 100))))
        ((temp2 i32) (new-shared-ptr
                       (new-i32 (i32.const 200)))))
    (with-destruct (temp1 temp2)
      (print-typed $&temp2)
      (set-local-shared-ptr temp2 temp1)
      (log (i32.const 111111))
      (print-typed $&temp1)
      (print-typed $&temp2)
      (log (i32.const 111222)))
    (log (i32.const 111333))))

(defun.wat cons.sp ((ptr1 i32) (ptr2 i32)) (i32)
  (new-shared-ptr
   (cons (new-shared-ptr ptr1)
         (new-shared-ptr ptr2))))

(defmacro.wat list.sp (&rest rest)
  (labels ((rec (rest)
             (if rest
                 `(cons.sp ,(car rest) ,(rec (cdr rest)))
                 `(get-null-ptr))))
    (rec rest)))

(defun.wat test-shared-ptr2 () ()
  (let (((lst i32) (cons.sp (new-i32 (i32.const 1))
                            (new-i32 (i32.const 2)))))
    (with-destruct (lst)
      (log (i32.const 222111))
      (print-typed $&lst))
    (log (i32.const 222222))))

(defun.wat test-shared-ptr3-called ((sp i32)) ()
  (with-destruct (sp)
    (log (i32.const 333999))
    (print-typed $&sp))
  (log (i32.const 333888)))

(defun.wat test-shared-ptr3 () ()
  (let (((sp i32) (new-shared-ptr (new-i32 (i32.const 1)))))
    (with-destruct (sp)
      (log (i32.const 333111))
      (test-shared-ptr3-called $&sp)
      (log (i32.const 333222)))
    (log (i32.const 333333))))

(defun.wat test-shared-ptr () ()
  (with-debug
    (test-shared-ptr1)
    (test-shared-ptr2)
    (test-shared-ptr3))
  (log (no-memory-allocated-p)) ; expect 1
  )

(defexport.wat test-shared-ptr (func test-shared-ptr))

;; --- env --- ;;

;; TODO: consider to use shared ptr instead of raw pointer

;; - env - ;;

;; env has a list of scope.
;; scope has a list of var-cell.
;; var-cell has pair of symbol and value.
(deftype.wat env 1 201)

(defun.wat new-env () (i32)
  (let (((ptr i32) (make-env)))
    (store-i32 (get-type-data-offset ptr)
               (get-null-ptr))
    (get-local ptr)))

(defun.wat add-variable-to-env ((env i32) (symbol i32) (value i32)) ()
  (if (empty-env-p env)
      (store-i32 (get-type-data-offset env)
                 (new-var-cell symbol value))
      (add-var-cell (get-head-var-cell env) symbol value)))

(defun.wat get-symbol-value ((env i32) (symbol i32)) (i32)
  (let ((var-cell i32)
        (result i32)
        (tmp-for-log i32))
    (cond ((empty-env-p env)
           (set-local result (get-null-ptr)))
          (t (set-local var-cell
                        (find-var-cell-by-symbol (get-head-var-cell env)
                                                 symbol))
             (if (null-ptr-p var-cell)
                 (set-local result (get-null-ptr))
                 (set-local result (get-var-cell-value* var-cell)))))
    (get-local result)))

(defun.wat get-head-var-cell ((env i32)) (i32)
  (load-i32 (get-type-data-offset env)))

(defun.wat empty-env-p ((env i32)) (i32)
  (null-ptr-p (get-head-var-cell env)))

(defun.wat free-env ((env i32)) ()
  (unless (empty-env-p env)
    (free-var-cell (get-head-var-cell env)))
  (free env))

;; - scope - ;;

(deftype.wat scope 1 202)

;; - var-cell - ;;

;; (variable-ptr value-ptr next-var-cell-ptr)
(deftype.wat var-cell 3 203)

(defun.wat new-var-cell ((s-symbol i32) (s-value i32)) (i32)
  (let (((ptr i32) (make-var-cell)))
    (with-destruct (s-symbol s-value)
      (store-i32 (get-type-data-offset ptr)
                 $&s-symbol)
      (store-i32 (i32+ (get-type-data-offset ptr) 1)
                 $&s-value)
      (store-i32 (i32+ (get-type-data-offset ptr) 2)
                 (sp (get-null-ptr))))
    (sp (get-local ptr))))

(defun.wat add-var-cell ((var-cell i32) (new-symbol i32) (new-value i32)) ()
  (let (((symbol i32) $&(get-var-cell-symbol* $*var-cell))
        (old i32))
    (with-destruct (var-cell new-symbol new-value symbol old)
      (cond ((eq-symbol $*symbol $*new-symbol)
             (set-local old (get-var-cell-value* $*var-cell))
             (set-var-cell-value $&var-cell $&new-value))
            ((next-var-cell-exist-p* $*var-cell)
             (add-var-cell $&(get-var-cell-next* $*var-cell)
                           $&new-symbol
                           $&new-value))
            (t (set-var-cell-next $&var-cell
                                  $&(new-var-cell $&new-symbol $&new-value)))))))

(defun.wat find-var-cell-by-symbol ((var-cell i32) (symbol i32)) (i32)
  (let ((result i32))
    (with-destruct (var-cell symbol)
      (cond ((eq-symbol $*(get-var-cell-symbol* $*var-cell) $*symbol)
             (set-local result $&var-cell))
            ((next-var-cell-exist-p* $*var-cell)
             (set-local result
                        (find-var-cell-by-symbol $&(get-var-cell-next* $*var-cell)
                                                 $&symbol)))
            (t (set-local result (sp (get-null-ptr))))))
    (get-local result)))

(defun.wat get-var-cell-symbol* ((ptr i32)) (i32)
  (load-i32 (get-type-data-offset ptr)))

(defun.wat get-var-cell-value* ((ptr i32)) (i32)
  (load-i32 (i32+ (get-type-data-offset ptr) 1)))

(defun.wat set-var-cell-value ((s-ptr i32) (s-value i32)) ()
  (with-destruct (s-ptr s-value)
    (store-i32 (i32+ (get-type-data-offset $*s-ptr) 1)
               $&s-value)))

(defun.wat get-var-cell-next* ((ptr i32)) (i32)
  (load-i32 (i32+ (get-type-data-offset ptr) 2)))

(defun.wat set-var-cell-next ((s-ptr i32) (next-ptr i32)) ()
  (let ((old i32))
    (with-destruct (s-ptr next-ptr old)
      (set-local old (get-var-cell-next* $*s-ptr))
      (store-i32 (i32+ (get-type-data-offset $*s-ptr) 2)
                 next-ptr))))

(defun.wat next-var-cell-exist-p* ((ptr i32)) (i32)
  (let ((result i32))
    (if (null-ptr-p $*(get-var-cell-next* ptr))
        (set-local result (i32.const 0))
        (set-local result (i32.const 1)))
    (get-local result)))

(defun.wat free-var-cell ((ptr i32)) ()
  (free-typed (get-var-cell-next* ptr))
  (free-typed (get-var-cell-symbol* ptr))
  (free-typed (get-var-cell-value* ptr))
  (free ptr))

;; - test - ;;

(defun.wat new-test-var-cell ((symbol-id i32) (i32-value i32)) (i32)
  (new-var-cell (sp (new-symbol symbol-id))
                (sp (new-i32 i32-value))))

(defun.wat test-env () ()
  (let ((tmp i32)
        (res1 i32)
        (res2 i32)
        (res3 i32)
        (tmp-for-log i32))
    (init-memory)
    (log-string "- var-cell -" tmp-for-log)
    (with-destruct (tmp res1 res2 res3)
      (set-local tmp (new-test-var-cell (i32.const 1) (i32.const 10)))
      ;; add
      (add-var-cell $&tmp
                    (sp (new-symbol (i32.const 2)))
                    (sp (new-i32 (i32.const 20))))
      ;; overwrite
      (add-var-cell $&tmp
                    (sp (new-symbol (i32.const 2)))
                    (sp (new-i32 (i32.const 30))))
      ;; check1
      (set-local res1 (find-var-cell-by-symbol
                       $&tmp (sp (new-symbol (i32.const 1)))))
      (log (get-i32 $*(get-var-cell-value* $*res1))) ; expect 10
      ;; check2
      (set-local res2 (find-var-cell-by-symbol
                          $&tmp (sp (new-symbol (i32.const 2)))))
      (log (get-i32 $*(get-var-cell-value* $*res2))) ; expect 30
      ;; check3
      (set-local res3 (find-var-cell-by-symbol
                       $&tmp (sp (new-symbol (i32.const 3)))))
      (log (null-ptr-p $*res3)) ; expect 1
      )
    (log (no-memory-allocated-p)) ; expect 1
    ))

(defexport.wat test-env (func test-env))

;; --- list interpreter --- ;;

(deftype.wat symbol 1 91)

(defun.wat new-symbol ((value i32)) (i32)
  (let (((ptr i32) (make-symbol)))
    (set-i32 ptr value)
    (get-local ptr)))

(defun.wat free-symbol ((ptr i32)) ()
  (free ptr))

(defun.wat get-symbol-id ((symbol-ptr i32)) (i32)
  (load-i32 (get-type-data-offset symbol-ptr)))

(defun.wat eq-symbol ((ptr1 i32) (ptr2 i32)) (i32)
  (i32.eq (get-symbol-id ptr1)
          (get-symbol-id ptr2)))

(defun.wat car.sp ((s-ptr i32)) (i32)
  (car $*s-ptr))

(defun.wat cdr.sp ((s-ptr i32)) (i32)
  (cdr $*s-ptr))

(defun.wat atom.sp ((s-ptr i32)) (i32)
  (atom $*s-ptr))

(defun.wat interpret-list ((s-ptr i32)) (i32)
  (let (((head i32) $&(car.sp s-ptr))
        ((rest i32) $&(cdr.sp s-ptr))
        (result i32)
        (tmp1 i32)
        (tmp2 i32)
        (symbol-id i32)
        (tmp-for-log i32))
    (with-destruct (s-ptr head rest tmp1 tmp2)
      (cond ((symbol-p $*head)
             (set-local symbol-id (get-symbol-id $*head))
             (cond (; 1: atom
                    (i32.eq symbol-id (i32.const 1))
                    (set-local tmp1 (interpret $&(car.sp rest)))
                    (set-local result
                               (sp (new-i32 (atom.sp tmp1)))))
                   (; 2: eq
                    (i32.eq symbol-id (i32.const 2))
                    (set-local tmp1 (interpret $&(car.sp rest)))
                    (set-local tmp2 (interpret $&(car.sp (cdr.sp rest))))
                    (set-local result
                               (sp (new-i32 (eq-typed $&tmp1 $&tmp2)))))
                   (; 3: car
                    (i32.eq symbol-id (i32.const 3))
                    (set-local tmp1 (interpret $&(car.sp rest)))
                    (set-local result $&(car.sp tmp1)))
                   (; 4: cdr
                    (i32.eq symbol-id (i32.const 4))
                    (set-local tmp1 (interpret $&(car.sp rest)))
                    (set-local result $&(cdr.sp tmp1)))
                   (; 5: cons
                    (i32.eq symbol-id (i32.const 5))
                    (set-local tmp1 (interpret $&(car.sp rest)))
                    (set-local tmp2 (interpret $&(car.sp (cdr.sp rest))))
                    (set-local result (cons.sp $&tmp1 $&tmp2)))
                   (; 101: quote
                    (i32.eq symbol-id (i32.const 101))
                    (set-local result $&(car.sp rest)))))
            (t (log-string "ERROR: head type: " tmp-for-log)
               (log (get-type $*head))))))
    (get-local result))

(defun.wat interpret ((s-ptr i32)) (i32)
  (let ((result i32))
    (with-destruct (s-ptr result)
      (cond ((atom $*s-ptr)
             (set-local result $&$&s-ptr))
            (t (set-local result $&(interpret-list $&s-ptr)))))
    (get-local result)))

(defun.wat test-list-interpreter () ()
  (let ((tmp i32)
        (result i32)
        (tmp-for-log i32))
    (init-memory)
    (progn                              ; with-debug
      (log-string "- 111" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (sp (new-i32 (i32.const 111))))
        (set-local result (interpret $&tmp))
        (print-typed $&result))
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (atom 10)" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 1))
                                (new-i32 (i32.const 10))))
        (set-local result (interpret $&tmp))
        (print-typed $&result))
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (eq 10 10)" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 2))
                                (new-i32 (i32.const 10))
                                (new-i32 (i32.const 10))))
        (set-local result (interpret $&tmp))
        (print-typed $&result))         ; expect 1
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (eq 10 20)" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 2))
                                (new-i32 (i32.const 10))
                                (new-i32 (i32.const 20))))
        (set-local result (interpret $&tmp))
        (print-typed $&result))         ; expect 0
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (quote (1 2))" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 101))
                                (list.sp (new-i32 (i32.const 1))
                                         (new-i32 (i32.const 2)))))
        (set-local result (interpret $&tmp))
        (print-typed $&(car.sp result))
        (print-typed $&(cdr.sp result)))
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (atom (quote (1 2)))" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 1))
                                (list.sp (new-symbol (i32.const 101))
                                         (list.sp (new-i32 (i32.const 100))
                                                  (new-i32 (i32.const 200))))))
        (set-local result (interpret $&tmp))
        (print-typed $&result))     ;; expect 0
      (log (no-memory-allocated-p)) ; expect 1

      (log-string "- (car (quote (100 . 200)))" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 3))
                                (list.sp (new-symbol (i32.const 101))
                                         (cons.sp (new-i32 (i32.const 100))
                                                  (new-i32 (i32.const 200))))))
        (set-local result (interpret $&tmp)) ;; expect 100
        (print-typed $&result))
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (cdr (quote (100 . 200)))" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 4))
                                (list.sp (new-symbol (i32.const 101))
                                         (cons.sp (new-i32 (i32.const 100))
                                                  (new-i32 (i32.const 200))))))
        (set-local result (interpret $&tmp)) ;; expect 200
        (print-typed $&result))
      (log (no-memory-allocated-p))     ; expect 1

      (log-string "- (car (cons 100 200)))" tmp-for-log)
      (with-destruct (tmp result)
        (set-local tmp (list.sp (new-symbol (i32.const 3))
                                (list.sp (new-symbol (i32.const 5))
                                         (new-i32 (i32.const 100))
                                         (new-i32 (i32.const 200)))))
        (set-local result (interpret $&tmp)) ;; expect 100
        (print-typed $&result))
      (log (no-memory-allocated-p))     ; expect 1
      )))

(defexport.wat test-list-interpreter (func test-list-interpreter))

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
  (log g))

;; factorial
(defun.wat test-rec ((x i32)) (i32)
  (let ((result i32))
    (if (i32.ge-u (i32.const 1) x)
        (set-local result (i32.const 1))
        (progn (i32.mul x
                        (test-rec (i32.sub x (i32.const 1))))
               (set-local result)))
    (get-local result)))

(defun.wat test-simple-log-string () ()
  (i32.store8 (i32.const 0) (i32.const 227))
  (i32.store8 (i32.const 1) (i32.const 129))
  (i32.store8 (i32.const 2) (i32.const 130))
  (i32.store8 (i32.const 3) (i32.const 97))
  (logs (i32.const 0) (i32.const 4)))

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
  (log (test-rec (i32.const 5)))
  (test-simple-log-string))

(defexport.wat exported-func (func test-print))

(defpackage :try-wasm-with-cl/wa/reserved-word
  (:use #:cl)
  (:export #:|module|

           #:|import|
           #:|export|
           #:|func|
           #:func
           #:|memory|
           #:memory
           #:|global|
           #:global
           #:|mut|
           #:mut
           #:|param|
           #:|result|

           #:|if|
           #:|then|
           #:then
           #:|else|
           #:else

           #:|block|
           #:block
           #:|loop|
           #:loop

           #:|i32|

           #:local)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :try-wasm-with-cl/wa/reserved-word)

(defmacro defrw (sym)
  (let ((lower-sym (intern (string-downcase (symbol-name sym)))))
    `(progn (defvar ,sym nil)
            (defvar ,lower-sym nil))))

(defrw module)

(defrw import)
(defrw export)
(defrw func)
(defrw memory)
(defrw global)
(defrw mut)
(defrw param)
(defrw result)

(defrw if)
(defrw then)
(defrw else)

(defrw block)
(defrw loop)

;; - type - ;;

(defrw i32)

;; - special form - ;;

(defrw local)

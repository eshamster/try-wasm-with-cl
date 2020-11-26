(defpackage :try-wasm-with-cl/wa/utils
  (:use #:cl)
  (:export #:clone-list-with-modification
           #:parse-arg-name
           #:symbol-to-string
           #:parse-mod-nm)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:split))
(in-package :try-wasm-with-cl/wa/utils)

(defun clone-list-with-modification (list fn-each-sym)
  (labels ((rec (rest)
             (if (atom rest)
                 (funcall fn-each-sym rest)
                 (mapcar (lambda (unit)
                           (rec unit))
                         rest))))
    (rec list)))

(defun parse-arg-name (arg-name)
  (symbolicate '$ arg-name))

(defun symbol-to-string (sym)
  (string-downcase
   (regex-replace-all "-" (symbol-name sym) "_")))

(defun parse-mod-nm (mod-nm)
  (let* ((*print-case* :downcase)
         (mod-nm-str (format nil "~A" mod-nm))
         (splitted (split "\\." mod-nm-str)))
    (unless (= (length splitted) 2)
      (error "mod-nm should be \"xxx.yyy\" but got ~A" mod-nm))
    splitted))

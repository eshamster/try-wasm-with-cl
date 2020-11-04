(defpackage :try-wasm-with-cl/src/js/utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :pse-create
           :get-layered-hash))
(in-package :try-wasm-with-cl/src/js/utils)

(defun map-pair (function list)
  "Ex. (map-pair (lambda (a b) (+ a b)) '(1 2 3 4)) => '(3 7)"
  (labels ((rec (rest result)
             (if rest
                 (rec (cddr rest)
                      (cons (funcall function (car rest) (cadr rest)) result))
                 result)))
          (nreverse (rec list nil))))

#|
Example:
(defvar *hash*
  (pse-create
   (:position (:x 12 :y (+ 10 20))
    :size (:width (* 2 3) :height 100)
    :some-list (list 1 2 3))))

(get-layered-hash *hash* :position :x)  => 12
(get-layered-hash *hash* :position :y)  => 30
(get-layered-hash *hash* :size :width)  => 6
(get-layered-hash *hash* :size :height) => 100
(get-layered-hash *hash* :some-list) => (1 2 3)
|#
(defmacro.ps+ pse-create (list)
  (labels ((pairp (element)
             (and (listp element)
                  (string= (package-name (symbol-package (car element)))
                           "KEYWORD")))
           (make-hash-insertion (rest)
             `(let ((result (make-hash-table)))
                ,@(map-pair (lambda (key value)
                              `(setf (gethash ,key result)
                                     ,(if (pairp value)
                                          (make-hash-insertion value)
                                          value)))
                             rest)
                   result)))
        (make-hash-insertion list)))

(defun.ps+ get-layered-hash (hash &rest keys)
  (labels ((rec (rest-keys result)
             (if rest-keys
                 (rec (cdr rest-keys)
                      (gethash (car rest-keys) result))
                 result)))
    (rec keys hash)))

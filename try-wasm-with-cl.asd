#|
  This file is a part of try-wasm-with-cl project.
  Copyright (c) 2020 eshamster (hamgoostar@gmail.com)
|#

#|
  try-wasm-with-cl is a sample project for WASM with Common Lisp.

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem try-wasm-with-cl
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "MIT"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack
               :cl-ppcre
               :try-wasm-with-cl/main)
  :description "try-wasm-with-cl is a sample project for WASM with Common Lisp."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op try-wasm-with-cl/t))))

(defsystem try-wasm-with-cl/t
  :class :package-inferred-system
  :depends-on (:ps-experiment
               :ps-experiment/t
               :rove)
  :perform (test-op (o c) (symbol-call :rove '#:run c)))

(defpackage :try-wasm-with-cl/server
  (:use :cl)
  (:export :start-server
           :stop-server)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :cl-markup
                :markup
                :html5)
  (:import-from :lack
                :builder)
  (:import-from :ningle
                :<app>
                :route)
  (:import-from :cl-ppcre
                :scan))
(in-package :try-wasm-with-cl/server)

(defvar *app* (make-instance '<app>))

(defvar *server* nil)

(defvar *script-dir*
  (merge-pathnames "static/"
                   (asdf:component-pathname
                    (asdf:find-system :try-wasm-with-cl))))

(setf (route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "try-wasm-with-cl")))))))

(defun stop-server ()
  (when *server*
    (stop *server*)
    (setf *server* nil)))

(defun start-server (&key (port 5000) (address "0.0.0.0"))
  (stop-server)
  (setf *server*
        (clackup
         (builder (:static :path (lambda (path)
                                   (when (scan "^(?:/images/|/css/|/js/|/wasm/|/robot\\.txt$|/favicon\\.ico$)"
                                               path)
                                     path))
                           :root *script-dir*)
                  *app*)
         :port port
         :address address)))

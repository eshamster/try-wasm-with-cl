(defpackage :try-wasm-with-cl/server
  (:use :cl)
  (:export :start-server
           :stop-server)
  (:import-from :try-wasm-with-cl/compiler
                :wat2wasm)
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

(defvar *wat-path*
  (merge-pathnames "wasm/main.wat" *script-dir*))

(defvar *wasm-path*
  (merge-pathnames "wasm/main.wasm" *script-dir*))

(setf (route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "try-wasm-with-cl")
                    (:script :src "js/main.js" nil)))))))

(defun stop-server ()
  (when *server*
    (stop *server*)
    (setf *server* nil)))

(defun start-server (&key (port 5000) (address "0.0.0.0"))
  (stop-server)
  (setf *server*
        (clackup
         (builder (lambda (app)
                    (lambda (env)
                      (let ((res (funcall app env))
                            (path (getf env :path-info)))
                        (when (scan "\\.wasm$" path)
                          (print 'test)
                          (wat2wasm *wat-path* *wasm-path*)
                          (setf (getf (cadr res) :content-type)
                                "application/wasm"))
                        res)))
                  (:static :path (lambda (path)
                                   (when (scan "^(?:/images/|/css/|/js/|/wasm/|/robot\\.txt$|/favicon\\.ico$)"
                                               path)
                                     path))
                           :root *script-dir*)
                  *app*)
         :port port
         :address address)))
